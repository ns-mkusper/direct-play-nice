#!/usr/bin/env python3
"""Export super-resolution weights to ONNX files usable with --ai-upscale-model custom.

Supported architectures:
  compact  Real-ESRGAN SRVGGNetCompact (realesr-animevideov3: --num-conv 16,
           realesr-general-x4v3 / -wdn-x4v3: --num-conv 32), 4x, BSD-3-Clause.
  span     SPAN (hongyuanyu/SPAN, Apache-2.0): official spanx2_ch48 / spanx4_ch48
           weights from OpenModelDB, feature_channels 48. Pass --span-arch pointing
           at basicsr/archs/span_arch.py from the SPAN repository.

Outputs opset 13, IR 7 graphs with one float32 NCHW RGB input named `input`
(dynamic height/width) and one output named `output`, which is what
direct_play_nice expects. Requires torch and onnx.

Examples:
  python3 export_sr_onnx.py compact --weights realesr-animevideov3.pth --num-conv 16 --out realesr-animevideov3.onnx
  python3 export_sr_onnx.py span --weights 2x-spanx2-ch48.pth --scale 2 --span-arch SPAN/basicsr/archs/span_arch.py --out span_x2.onnx
"""
import argparse
import hashlib
import sys
import types

import torch
import torch.nn as nn
import torch.nn.functional as F


class SRVGGNetCompact(nn.Module):
    """Real-ESRGAN compact network (realesr-*-x4v3, realesr-animevideov3)."""

    def __init__(self, num_feat=64, num_conv=16, upscale=4):
        super().__init__()
        self.upscale = upscale
        self.body = nn.ModuleList()
        self.body.append(nn.Conv2d(3, num_feat, 3, 1, 1))
        self.body.append(nn.PReLU(num_parameters=num_feat))
        for _ in range(num_conv):
            self.body.append(nn.Conv2d(num_feat, num_feat, 3, 1, 1))
            self.body.append(nn.PReLU(num_parameters=num_feat))
        self.body.append(nn.Conv2d(num_feat, 3 * upscale * upscale, 3, 1, 1))
        self.upsampler = nn.PixelShuffle(upscale)

    def forward(self, x):
        out = x
        for layer in self.body:
            out = layer(out)
        out = self.upsampler(out)
        base = F.interpolate(x, scale_factor=self.upscale, mode="nearest")
        return out + base


def load_state_dict(path):
    sd = torch.load(path, map_location="cpu")
    if isinstance(sd, dict):
        for key in ("params_ema", "params"):
            if key in sd:
                return sd[key]
    return sd


def load_span_class(arch_path):
    src = open(arch_path).read()
    src = src.replace("from basicsr.utils.registry import ARCH_REGISTRY", "")
    src = src.replace("@ARCH_REGISTRY.register()", "")
    module = types.ModuleType("span_arch")
    exec(src, module.__dict__)
    return module.SPAN


def export(net, out_path):
    net.eval()
    dummy = torch.rand(1, 3, 64, 64)
    with torch.no_grad():
        torch.onnx.export(
            net,
            dummy,
            out_path,
            input_names=["input"],
            output_names=["output"],
            opset_version=13,
            dynamic_axes={"input": {2: "height", 3: "width"}, "output": {2: "height_out", 3: "width_out"}},
            dynamo=False,
        )
    try:
        import onnx

        onnx.checker.check_model(onnx.load(out_path))
    except ImportError:
        print("onnx package not installed; skipping graph check", file=sys.stderr)
    digest = hashlib.sha256(open(out_path, "rb").read()).hexdigest()
    print(f"wrote {out_path}\nsha256 {digest.upper()}")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("arch", choices=["compact", "span"])
    parser.add_argument("--weights", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--scale", type=int, default=4)
    parser.add_argument("--num-conv", type=int, default=16, help="compact only: 16 for animevideov3, 32 for general-x4v3")
    parser.add_argument("--num-feat", type=int, default=64, help="compact only")
    parser.add_argument("--span-arch", help="span only: path to span_arch.py from the SPAN repository")
    parser.add_argument("--span-channels", type=int, default=48, help="span only: feature_channels")
    args = parser.parse_args()

    sd = load_state_dict(args.weights)
    if args.arch == "compact":
        net = SRVGGNetCompact(num_feat=args.num_feat, num_conv=args.num_conv, upscale=args.scale)
    else:
        if not args.span_arch:
            parser.error("--span-arch is required for span")
        span = load_span_class(args.span_arch)
        net = span(
            num_in_ch=3,
            num_out_ch=3,
            feature_channels=args.span_channels,
            upscale=args.scale,
            bias=True,
            img_range=255.0,
            rgb_mean=(0.4488, 0.4371, 0.4040),
        )
    net.load_state_dict(sd, strict=True)
    export(net, args.out)


if __name__ == "__main__":
    main()
