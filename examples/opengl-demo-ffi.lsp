#!/usr/bin/env newlisp 
# OpenGL and GLUT demo - opengl-demo-ffi.lsp
# Using extended import and callback API based on libffi
# tested on Windows and OS X 32/64-bit (Intel) minimum version newLISP 10.3.10

# verson 2.1 December 2011 - remade with extended FFI for 32 and 64-bit
# verson 2.2 February 2012 - check if newLISP compiled for extended FFI
# verson 2.3 April 2015 - changed ostype to Windows from Win32
# this version works on both, 32-bit and 64-bit newLISP
# on a Mac OSX standard install everything neede for 32-bit or 64-bit
# is included. On Windows XP and Windows 7, glut32.dll must be added. 
# on Linux opengl.so, libglut.so and libffi.so must be added, most
# Linux seem to include libffi.so already. See also the C version:
# http://graphics.stanford.edu/courses/cs248-01/OpenGLHelpSession/code_example.html

#
# for a complete function reference for OpenGL look here:
#     http://www.opengl.org/documentation/
#
# for descriptions and refence to GLU see here:
#     http://www.opengl.org/resources/libraries/glut/
#
# Calling patterns for OpenGL functions:
# Functions are described in the manual with the following types:
#
# All the following integer types are passed as is or with (int ...):
# GLenum GLboolean GLbitfield GLbyte GLshort GLint GLsizei GLubyte GLushort GLuint
# and types ending in: 3b, si, 3s
#
# The following double float types are passed as is or with (float ...):
# GLdouble GLclampd
# and types ending in: 3d
#
# The following are small float types
# GLfloat GLclampf
# and all types ending 3f:
# 
# Note! on Windows 7 glut32.dll should be installed c:/Windows/SysWOW64/ not system32

(set 'is-64-bit (= 0x100 (& 0x100 (sys-info -1))))
(set 'has-ffi (= 1024 (& 1024 (sys-info -1))))

(unless has-ffi
    (println "need newLISP compiled for extended FFI")
    (exit))

(if 
  (= ostype "OSX") ;; drawings will be visible only on x86 based OS X
  (begin
    (set 'GL_LIB "/System/Library/Frameworks/OpenGL.Framework/Libraries/libGL.dylib")
    (set 'GLUT_LIB "/System/Library/Frameworks/GLUT.Framework/GLUT")
  )
  (find ostype '("Windows", "Cygwin"))
  (begin
    (set 'GL_LIB "opengl32.dll") ; works dfor both 32 and 64 bit
    (set 'GLUT_LIB (if is-64-bit "glut64.dll" "glut32.dll")))
  (= ostype "Linux") ;; not tested
  (begin
    (set 'GL_LIB "libGL.so")
    (set 'GLUT_LIB "libglut.so"))
)

(import GL_LIB "glClear" "void" "int")
(import GL_LIB "glClearColor" "void" "float" "float" "float" "float")
(import GL_LIB "glEnable" "void" "int")
(import GL_LIB "glHint" "void" "int" "int")
(import GL_LIB "glColor3d" "void" "double" "double" "double")
(import GL_LIB "glBegin" "void" "int")
(import GL_LIB "glEnd" "void" "void")
(import GL_LIB "glVertex3d" "void" "double" "double" "double")
(import GL_LIB "glFlush" "void")
(import GL_LIB "glFinish" "void")
(import GL_LIB "glRotated" "void" "double" "double" "double" "double")
(import GL_LIB "glLineWidth" "void" "float")

(import GLUT_LIB "glutInit" "void" "void*" "void*")
(import GLUT_LIB "glutDisplayFunc" "void" "void*")
(import GLUT_LIB "glutInitWindowSize" "void" "int" "int")
(import GLUT_LIB "glutInitDisplayMode" "void" "int")
(import GLUT_LIB "glutInitWindowPosition" "void" "int" "int")
(import GLUT_LIB "glutDisplayFunc" "void" "void*")
(import GLUT_LIB "glutKeyboardFunc" "void" "void*")
(import GLUT_LIB "glutMouseFunc" "void" "void*")
(import GLUT_LIB "glutJoystickFunc" "void" "void*" "int")
(import GLUT_LIB "glutMotionFunc" "void" "void*")
(import GLUT_LIB "glutIdleFunc" "void" "void*")
(import GLUT_LIB "glutCreateWindow" "int" "char*")
(import GLUT_LIB "glutCreateSubWindow" "int" "int" "int" "int" "int" "int")
(import GLUT_LIB "glutMainLoop" "void")
(import GLUT_LIB "glutSwapBuffers" "void")
(import GLUT_LIB "glutPostRedisplay" "void")

(import GLUT_LIB "glutSolidCube" "void" "double")
(import GLUT_LIB "glutSolidTeapot" "void" "double")
(import GLUT_LIB "glutWireCube" "void" "double")
(import GLUT_LIB "glutWireTeapot" "void" "double")

(constant 'GL_COLOR_BUFFER_BIT 0x00004000)
(constant 'GL_TRIANGLES 0x0004)
(constant 'GL_BLEND 0x0BE2)
(constant 'GL_LINE_SMOOTH 0x0B20)
(constant 'GL_DONT_CARE 0x1100)
(constant 'GL_LINE_SMOOTH_HINT 0x0C52)

(constant 'GLUT_DOUBLE 0x0002)
(constant 'GLUT_SINGLE 0x0000)
(constant 'GLUT_RGB 0x0000)
(constant 'GLUT_RGBA 0x0000)

(set 'argc 0)
(set 'argv1 "")
(set 'argv (pack "lu" argv1))
(set 'rotx 0.0)
(set 'roty 0.0)
(set 'PI (atan2 0 -1))

(define (draw) 
	(glClear GL_COLOR_BUFFER_BIT)
	(glRotated rotx 0.0 1.0 0.0)
	(glRotated roty 1.0 0.0 0.0)
	(glutWireTeapot 0.5)
	(glutSwapBuffers)
)

(define (drawObject)
	(glClear GL_COLOR_BUFFER_BIT )
	(glColor3d 1.0 0.85 0.35)
	(glRotated rotx 0.0 1.0 0.0)
	(glRotated roty 1.0 0.0 0.0)
	(glBegin GL_TRIANGLES)
	(glVertex3d 0.0 0.6 0.0)
	(glVertex3d -0.2 -0.3 0.0)
	(glVertex3d 0.2 -0.3 0.0)
	(glEnd)
	(glutSwapBuffers)
)

(define (rotation)
	(set 'rotx (mod (sub rotx .01) PI))
	(set 'roty (mod (sub roty .012) PI))
	(sleep 10)
	(glutPostRedisplay)
)

(define (keyboard key x y)
	(if (= key 27) (exit)) 
	(println "key:" (& key 0xFF) " x:" x  " y:" y))

(define (mouse button state x y)
	(if (= state 0)
		(glutIdleFunc 0) ; auto-motion off on button press
		(glutIdleFunc (callback 'rotation))) ; auto-motion on
	(println "mouse button: " button " state:" state " x:" x " y:" y))

(define (joystick button x y z)
	(println "joystick button: " button " x: " x " y: " y " z: " z))

(define (motion x y)
	(set 'rotx (mul (div 200 x) PI))
	(set 'roty (mul (div 150 y) PI))
	(glutPostRedisplay)
	(println "x:" x  " y:" y))

(glutInit (address argc) (address argv))
(glutInitDisplayMode (| GLUT_RGB GLUT_DOUBLE ))
(glutInitWindowSize 800 600)
(glutInitWindowPosition 80 80)
(set 'id (glutCreateWindow "OpenGL and newLISP - drag mouse - ESC to exit"))

(glClearColor 0.5  0.3  0.3 0.0)
(glColor3d 1.0 0.85 0.35)

(glutDisplayFunc (callback 'draw "void"))
;(glutDisplayFunc (callback 0 'drawObject))
(glutKeyboardFunc (callback 'keyboard "void" "byte" "int" "int"))
(glutMouseFunc (callback 'mouse "void" "int" "int" "int" "int"))
(glutMotionFunc (callback 'motion "void" "int" "int"))
(glutIdleFunc (callback 'rotation "void"))
(glutJoystickFunc (callback 'joystick "void" "int" "int" "int" "int") 50)
(glutMainLoop)

;; eof

;(glEnable GL_BLEND)
;(glEnable GL_LINE_SMOOTH) ; turn on antialiasing
;(glHint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
;(glLineWidth 3)
