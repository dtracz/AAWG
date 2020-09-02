#ifndef TOOLKIT_H_
#define TOOLKIT_H_

#include <vector>
#include <memory>
#include <opencv2/imgcodecs.hpp>
#include "Frame.h"
#include "Net.h"


std::ostream& operator<<(std::ostream&, Frame);


class FrameClass {
  private:
    float _threshold = 0.80;
    std::shared_ptr<std::vector<Frame>> _framesPtr;

    float _getOverlapArea(Frame&, Frame&);

  public:
    FrameClass() = default;
    inline FrameClass(Frame frame): _framesPtr(std::make_shared<std::vector<Frame>>()) {
        _framesPtr->push_back(frame);
    }

    FrameClass add(Frame);

    inline Frame get() const {
        return _framesPtr->at(0);
    }

    inline bool operator==(FrameClass other) const {
        return _framesPtr == other._framesPtr;
    }
    inline bool operator!=(FrameClass other) const {
        return _framesPtr != other._framesPtr;
    }

};  // class FrameClass


class NetRunner {
  private:
    Net* _net;
    std::vector<cv::Mat> _imgs;

    NetRunner(): _net(nullptr) { }
    NetRunner(NetRunner&&) = default;
    NetRunner& operator=(NetRunner&&) = default;

  public:
    static NetRunner& getNetRunner(const char*);

    NetRunner(const NetRunner&) = delete;
    NetRunner& operator=(const NetRunner&) = delete;

    int32_t pushImg(const char*);

    int32_t runNet(Frame* frames);
      
};  // class NetRunner;


#endif  // TOOLKIT_H_
