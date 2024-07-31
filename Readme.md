[//]: # (Project readme template from https://github.com/othneildrew/Best-README-Template/)
<a name="readme-top"></a>
[![OCaml][ocaml_img]][ocaml_url]
[![risc-v][risc_img]][risc_url]
[![License][license_img]][repo_license_url]
[<img alt="logo" src="resources/logo.png" width="100" height="100" align="right">](https://github.com/CrocodielRussian/Compiler)


<h1 align="left">ChimeraLang Compiler</h1>

## Description

This project is simple compiler of programmer language `ChimeraLang`. It do the parse of `ChimeraLang` code and build AST of that. After, It translte that AST in [`RISC-V`][risc_url] AST. In finnally, it generate [`RISC-V`][risc_url] code for execute of programm.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## `ChimeraLang` grammar
### Expressions
```grammar
<add-operation> = + | -
<mult-operation> = * | / | %
<integer> = [0-9]+
<indetifier> = [a-zA-Z_]+
<unary-expression> = +<unit-expression> | -<unit-expression> | !<unit-expression>
<unit-expression> = <unary> | (<expression>) | <integer> | <indetifier> | <func-call>
<func-call> = <indetifier>(<expression-list>)
<mult-expression> = <unary-expression> | <unary-expression><mult-operation><mult-expression>
<math-expression> = <mult-expression> | <mult-expression><add-operation><math-expression>
<compare-expression> = <math-expression> | <math-expression><compare-operation><compare-expression>
<bool-operation> = && | ||
<bool-expression> = <compare-expression> | <compare-expression><bool-operation><bool-expression>
<assign-operation> = := | += | -= | *= | /=
<assign-expression> = <indetifier><assign-operation><expression>
<expression> = <bool-expression> | <assign-expression>
<expression-list> = e | <expression>,<expression-list>
```

### Statements
```grammar
<break-statement> = break
<return-statement> = return <expression>;
<if-statement> = if <expression> then <statement-list> else <statement-list> endif
<while-statement> = while <expression> do <statement-list> done
<expression-statement> = <expression>;
<var-init-statement> = var <indetifier> := <expression>;
<statement> = <var-init-statement> | <expression-statement> | <while-statement> | <if-statement> | <return-statement> | <break-statement>
<statement-list> = e | <statement><statement-list>
```

### Structures
```grammar
<func-define-struct> = def <indetifier>(var-list){<statement-list>}
<struct> = <func-define-struct>
<struct-list> = e | <struct-list>
```

### Program
```grammar
<program> = <struct-list>
```


## Getting started

To run building application, and execute it commands:

### Clone this repo
```bash
git clone https://github.com/CrocodielRussian/Compiler
```

### Download dependencies
#### RISC-V AS and LD, GDB Multiarch, qemu-user 
```bash
sudo apt install --yes binutils-riscv64-linux-gnu gdb-multiarch qemu-user
```

### Build
```bash
dune build
```

### Create source `bin/main.clang` and write some *`ChimeraLang`* code

```clang
def main() {
	var a := read_int();
	var b := read_int();
	print_int(a + b);
	return 0;
}
```

### Execution
```bash
dune exec compiler -- bin/main.clang lang/main.s --compile
```
```bash
make run
```
### Tests
```bash
dune test
```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Code examples
### Summary of two numbers
```clang
def main() {
	var a := read_int();
	var b := read_int();
	print_int(a + b);
	return 0;
}
```
### Summary of 1 to n numbers
```clang
def nsum(n) {
	if n <= 0 then 
		return 0;
	endif
	return n + nsum(n - 1);
}

def main() {
	var n := read_int();
	print_int(nsum(n));
	return 0;
}
```
### Factorial of n
```clang
def factorial(n) {
	var acc:=1;
	while n>1 do
		acc:=acc*n;
		n:=n-1;
	done
	return acc;
}

def main() {
	var n := read_int();
	print_int(factorial(n));
	return 0;
}
```
### Fibonacci of n
```clang
def fibonachi(n) {
	var a:=0; var b:=1;
	while n>1 do
		b:=a+b;
		a:=b-a;
		n:=n-1;
	done
	return b;
}

def main() {
	var n := read_int();
	print_int(fibonachi(n));
	return 0;
}
```
<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Authors

- [@IliaSuponeff](https://github.com/IliaSuponeff)
- [@CrocodielRussian](https://github.com/CrocodielRussian)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## License

Distributed under the [MIT License](https://choosealicense.com/licenses/mit/). See [`LICENSE`](LICENSE) for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- Image links -->

[ocaml_img]: https://img.shields.io/badge/OCaml-%204.13.1-magenta
[risc_img]: https://img.shields.io/badge/RISC-V-blue
[license_img]: https://img.shields.io/badge/License-MIT-green.svg

<!-- Inner Links -->

[repo_license_url]: https://github.com/spbu-coding-2023/graphs-graphs-12/blob/main/LICENSE

<!-- Outer Links -->

[ocaml_url]: https://ocaml.org/
[risc_url]: https://riscv.org/
