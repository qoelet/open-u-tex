# open-u-tex

A simple CLI for creating a TeX template for Open University assignments.

```shell
$ stack install

$ echo "{
  fullName = "Your name",
  studentId = "Your student id",
  email = "Your email"
}" >> .student

$ open-u-tex --module-name FOO123 --assignment BAR01 --questions 5 --save-to ./Foo.tex
```
