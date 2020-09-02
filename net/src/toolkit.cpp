#include <algorithm>
#include <vector>
#include <memory>
#include <ostream>
#include "Frame.h"
#include "./toolkit.h"


std::ostream& operator<<(std::ostream& os, Frame f) {
    os << f.label << " " << f.prob << " :\t"
       << f.pos[0] << '\t'<<  f.pos[1] << '\t'
       << f.pos[2] << '\t' << f.pos[3];
    return os;
}


float FrameClass::_getOverlapArea(Frame& f1, Frame& f2) {
    Frame overlap(0, 0, std::max(f1.pos[0], f2.pos[0]),
                        std::max(f1.pos[1], f2.pos[1]),
                        std::min(f1.pos[2], f2.pos[2]),
                        std::min(f1.pos[3], f2.pos[3]));
    return overlap.getArea() / (f1.getArea() + f2.getArea() - overlap.getArea());
}


FrameClass FrameClass::add(Frame frame) {
    if (_framesPtr->at(0).label != frame.label)
        return FrameClass(frame);
    if (_getOverlapArea(_framesPtr->at(0), frame) < _threshold)
        return FrameClass(frame);
    if (_framesPtr->at(0).prob < frame.prob)
        std::swap((*_framesPtr)[0], frame);
    _framesPtr->push_back(frame);
    return *this;
}



NetRunner& NetRunner::getNetRunner(const char* model_path) {
    static NetRunner netRunner;
    if (netRunner._net == nullptr)
        netRunner._net = new Net(model_path);
    return netRunner;
}


int32_t NetRunner::pushImg(const char* img_path) {
    auto&& img = cv::imread(img_path, cv::IMREAD_COLOR);
    std::remove(img_path);
    _imgs.push_back(img);
    return _imgs.size();
}


int32_t NetRunner::runNet(Frame* frames) {
    int32_t n_imgs = _imgs.size();
    auto vframes = (*_net)(_imgs);
    for (int i = 0; i < n_imgs; i++) {
        auto bestFrame = *std::max_element(vframes[i].begin(),
                                           vframes[i].end(),
            [](const Frame& f1, const Frame& f2) {
                return (f1.label == 3)*f1.prob < (f2.label == 3)*f2.prob;
            });
        frames[i] = bestFrame.label == 3 ? bestFrame : Frame(0,0,0,0,0,0);
    }
    _imgs.clear();
    return n_imgs;
}


