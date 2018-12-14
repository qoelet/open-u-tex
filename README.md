# open-u-tex

A simple CLI for creating a LaTeX template for Open University assignments, including

- numbering equations
- cover page
- page numbering
- footer with student name and id

See the generated `.tex` file for more details.

```shell
$ stack install

$ echo "{
  fullName = "Your name",
  studentId = "Your student id",
  email = "Your email"
}" >> .student

$ open-u-tex --module-name FOO123 --assignment BAR01 --questions 5 --save-to ./Foo.tex
```
