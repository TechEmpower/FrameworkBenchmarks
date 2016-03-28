#ifndef ROOT_H
#define ROOT_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class Root : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit Root(QObject *parent = 0);
    ~Root();

private:
    C_ATTR(End, :AutoArgs)
    void End(Context *c);
};

#endif //ROOT_H

