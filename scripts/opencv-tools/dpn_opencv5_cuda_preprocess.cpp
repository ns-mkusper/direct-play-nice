#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <exception>
#include <stdexcept>
#include <string>

#include <opencv2/core.hpp>
#include <opencv2/core/cuda.hpp>
#include <opencv2/cudaarithm.hpp>
#include <opencv2/cudafilters.hpp>
#include <opencv2/cudaimgproc.hpp>
#include <opencv2/imgproc.hpp>

namespace {

constexpr int DPN_MODE_BASIC = 1;
constexpr int DPN_MODE_SUBTITLE = 2;

void write_error(char *err, std::size_t err_len, const std::string &message) {
    if (err == nullptr || err_len == 0) {
        return;
    }
    const auto copy_len = std::min(err_len - 1, message.size());
    std::memcpy(err, message.data(), copy_len);
    err[copy_len] = '\0';
}

void validate_args(const std::uint8_t *pixels,
                   std::size_t pixels_len,
                   int width,
                   int height,
                   int mode,
                   std::uint8_t *output,
                   std::size_t output_len) {
    if (pixels == nullptr) {
        throw std::runtime_error("input pixel pointer is null");
    }
    if (output == nullptr) {
        throw std::runtime_error("output pixel pointer is null");
    }
    if (width <= 0 || height <= 0) {
        throw std::runtime_error("image dimensions must be positive");
    }
    const auto expected = static_cast<std::size_t>(width) * static_cast<std::size_t>(height);
    if (pixels_len != expected) {
        throw std::runtime_error("input pixel length does not match image dimensions");
    }
    if (output_len < expected) {
        throw std::runtime_error("output buffer is smaller than image dimensions");
    }
    if (mode != DPN_MODE_BASIC && mode != DPN_MODE_SUBTITLE) {
        throw std::runtime_error("unknown OpenCV CUDA preprocess mode");
    }
}

cv::cuda::GpuMat gaussian_3x3(const cv::cuda::GpuMat &src) {
    cv::cuda::GpuMat blurred;
    auto filter = cv::cuda::createGaussianFilter(CV_8UC1, CV_8UC1, cv::Size(3, 3), 0.0);
    filter->apply(src, blurred);
    return blurred;
}

cv::cuda::GpuMat binary_threshold(const cv::cuda::GpuMat &src, double threshold) {
    cv::cuda::GpuMat thresholded;
    cv::cuda::threshold(src, thresholded, threshold, 255.0, cv::THRESH_BINARY);
    return thresholded;
}

cv::cuda::GpuMat close_2x2(const cv::cuda::GpuMat &src) {
    cv::cuda::GpuMat closed;
    const cv::Mat kernel = cv::getStructuringElement(cv::MORPH_RECT, cv::Size(2, 2));
    auto close = cv::cuda::createMorphologyFilter(cv::MORPH_CLOSE, CV_8UC1, kernel);
    close->apply(src, closed);
    return closed;
}

} // namespace

extern "C" int dpn_opencv5_cuda_device_count(char *err, std::size_t err_len) {
    try {
        return cv::cuda::getCudaEnabledDeviceCount();
    } catch (const std::exception &ex) {
        write_error(err, err_len, ex.what());
        return -1;
    } catch (...) {
        write_error(err, err_len, "unknown OpenCV CUDA device-count failure");
        return -1;
    }
}

extern "C" const char *dpn_opencv5_cuda_build_info() {
    static const std::string info = std::string("OpenCV ") + CV_VERSION + " CUDA OCR preprocess shim";
    return info.c_str();
}

extern "C" int dpn_opencv5_cuda_preprocess_gray8(const std::uint8_t *pixels,
                                                  std::size_t pixels_len,
                                                  int width,
                                                  int height,
                                                  int mode,
                                                  std::uint8_t *output,
                                                  std::size_t output_len,
                                                  char *err,
                                                  std::size_t err_len) {
    try {
        validate_args(pixels, pixels_len, width, height, mode, output, output_len);
        const auto expected = static_cast<std::size_t>(width) * static_cast<std::size_t>(height);

        const cv::Mat host(height, width, CV_8UC1, const_cast<std::uint8_t *>(pixels));
        cv::cuda::GpuMat gpu_src;
        gpu_src.upload(host);

        cv::cuda::GpuMat result;
        if (mode == DPN_MODE_BASIC) {
            const auto blurred = gaussian_3x3(gpu_src);
            result = binary_threshold(blurred, 127.0);
        } else {
            const auto blurred = gaussian_3x3(gpu_src);
            const auto thresholded = binary_threshold(blurred, 96.0);
            result = close_2x2(thresholded);
        }

        cv::Mat host_out;
        result.download(host_out);
        if (!host_out.isContinuous()) {
            host_out = host_out.clone();
        }
        std::memcpy(output, host_out.data, expected);
        return 0;
    } catch (const cv::Exception &ex) {
        write_error(err, err_len, std::string("OpenCV CUDA error: ") + ex.what());
        return -1;
    } catch (const std::exception &ex) {
        write_error(err, err_len, ex.what());
        return -1;
    } catch (...) {
        write_error(err, err_len, "unknown OpenCV CUDA preprocess failure");
        return -1;
    }
}
