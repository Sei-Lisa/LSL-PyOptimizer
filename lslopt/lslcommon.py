# These types just wrap the Python types to make type() work on them.
# There are no ops defined on them or anything.

class Key(unicode):
    def __repr__(self):
        return self.__class__.__name__ + '(' + super(self.__class__, self).__repr__() + ')'

class Vector(tuple):
    def __repr__(self):
        return self.__class__.__name__ + '(' + super(self.__class__, self).__repr__() + ')'

class Quaternion(tuple):
    def __repr__(self):
        return self.__class__.__name__ + '(' + super(self.__class__, self).__repr__() + ')'

# Recognized: 3763, 6466, 6495
Bugs = set([6495])

LSO = False
