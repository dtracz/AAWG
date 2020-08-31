#ifndef FRAME_H_
#define FRAME_H_

#include <stdint.h>


struct Frame {
    int32_t label;
    float prob;
    float pos[4];

    inline Frame(int label, float prob,
                 float w0, float h0, float w1, float h1):
        label(label), prob(prob), pos{w0, h0, w1, h1} { }

    inline float getArea() {
        return (pos[2] - pos[0]) * (pos[3] - pos[1]);
    }

};  // struct Frame


#endif  // FRAME_H_
