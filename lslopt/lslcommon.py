#    (C) Copyright 2015 Sei Lisa. All rights reserved.
#
#    This file is part of LSL PyOptimizer.
#
#    LSL PyOptimizer is free software: you can redistribute it and/or
#    modify it under the terms of the GNU General Public License as
#    published by the Free Software Foundation, either version 3 of the
#    License, or (at your option) any later version.
#
#    LSL PyOptimizer is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with LSL PyOptimizer. If not, see <http://www.gnu.org/licenses/>.

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
