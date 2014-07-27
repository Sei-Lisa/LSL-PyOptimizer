# These types just wrap the Python types to make type() work on them.
# There are no ops defined on them or anything.

class Key(unicode):
    def __repr__(self):
        return 'Key(' + super(Key, self).__repr__() + ')'

class Vector(tuple):
    def __repr__(self):
        return 'Vector(' + super(Vector, self).__repr__() + ')'

class Quaternion(tuple):
    def __repr__(self):
        return 'Quaternion(' + super(Quaternion, self).__repr__() + ')'

# Recognized: 3763, 6466, 6495
Bugs = set([6495])

LSO = False
