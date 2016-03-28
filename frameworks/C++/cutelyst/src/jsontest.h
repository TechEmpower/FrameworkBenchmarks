#ifndef JSONTEST_H
#define JSONTEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class JsonTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit JsonTest(QObject *parent = 0);

    C_ATTR(json, :Local :AutoArgs)
    void json(Context *c);
};

#endif // JSONTEST_H
