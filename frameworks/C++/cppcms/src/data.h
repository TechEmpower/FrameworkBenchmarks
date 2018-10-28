#ifndef DATA_H
#define DATA_H

#include <cppcms/view.h>

#include <string>
#include <vector>

class fortune {
public:
    int id;
    std::string message;
};

typedef std::vector<fortune> fortune_vector;

class data_holder : public cppcms::base_content {
public:
    fortune_vector fortunes;
};



#endif /* DATA_H */

