#ifndef NET_H_
#define NET_H_

#include <vector>
#include <opencv2/imgcodecs.hpp>
#include "torch/csrc/api/include/torch/types.h"
#include "torch/csrc/jit/serialization/import.h"
#include "Frame.h"


class Net {
  private:
    torch::jit::script::Module _module;

    cv::Size _inp_size = cv::Size(300, 300);
    torch::Tensor _means = torch::full({3}, 127.0);
    float _stdev = 128.0;

    torch::Tensor _cvprepare(std::vector<cv::Mat>&);

    size_t _filter_results(torch::Tensor*, torch::Tensor*,
                           float threshold = 0.5);

    std::vector<Frame> _parse_frames(const torch::Tensor&,
                                     const torch::Tensor&);

  public:
    inline Net(std::string mod_path) {
        _module = torch::jit::load(mod_path);
    }

    std::vector<std::vector<Frame>> operator()(std::vector<cv::Mat>&);

};  // class Net


#endif  // NET_H_
