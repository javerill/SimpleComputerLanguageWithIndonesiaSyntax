#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <string.h>

using namespace std;

//Antonius

ifstream diskfile("info.pas");

const int norw = 11;		//Jumlah Reserved Word
const int txmax = 100;		//Panjang Tabel Identifier
const int nmax = 14;		//Max jumlah digit dalam angka
const int a1 = 10;			//Panjang Identifier
const int chsetsize = 128;	//Untuk ASCII char.Set
const int maxerr = 30;		//Maksimal Jumlah Errror
const int amax = 2048;		//Maksimal Alamat
const int levmax = 3;		//Maks kedalaman Jaringan Blok
const int cxmax = 200;		//Ukuran kode Array

enum symbol {
	nul, ident, number, plus, minus, times, slash, oddsym, eql, neq,
	lss, leq, gtr, geq, lparen, rparen, comma, semicolon, period,
	becomes, beginsym, endsym, ifsym, thensym, whilesym, dosym,
	callsym, constsym, varsym, procsym
};

typedef char alfa[a1];

enum obj { constant, variable, prozedure };

typedef symbol symset[50];

enum fct { lit, opr, lod, sto, cal, intg, jmp, jpc }; //Fungsi2

int testcount = 0, statcount = 0, decount = 0, facount = 0;

struct instruction
{
	enum fct f;			//Kode Fungsi		
	int l;				//Level (0-3)
	int a;				//Alamat Jarak (0-2048)
};

char ch;	//Baca karakter Terakhir
symbol sym;	//Baca Symbol Terakhir
alfa id;	//Baca identifier terakhir
int num;	//baca angka terakhir
int cc;		//hitung karakter
int ll;		//panjang baris
int kk;
int err;
int cx;		//indeks alokasi kode
int tx, lev, dx, cx0;
char line[81];
alfa a;
instruction code[cxmax];
alfa word[norw];
enum symbol wsym[norw];
enum symbol ssym[sizeof(char)];
char mnemonic[sizeof(fct)][5];

symset declbegsys, statbegsys, facbegsys;

struct str
{
	alfa name;
	enum obj kind;
	int val;
	int level;
	int adr;
	int size;
};

str table[txmax];

void gen(fct x, int y, int z);
void enter(obj k);
int position(alfa id);
void constdeclaration();
void vardeclaration();
void listcode();
void factor(symset fsys);
void term(symset fsys);
void expression(symset fsys);
void condition(symset fsys);
bool infsys(symset fsys, symbol sym);
void statement(symset fsys);
void block(int lev, int tx, symset fsys);
bool indeclbegsys(symbol sym);
void interpret();
bool odd(int temp);

void printMenu()
{
	cout << "=================================================" << endl;
	cout << "A Simple Computer Language With Indonesian Syntax" << endl;
	cout << "=================================================" << endl;
	cout << "1. Load file and run the program" << endl;
	cout << "2. Exit program" << endl;
	cout << "Choice: " << endl;
}

bool odd(int temp)
{
	if (temp % 2 == 1) return true;
	else return false;
}

void error(int n)
{
	printf(" :%d          error code=  %d", cc - 1, n);
	scanf_s("%c", &ch);
	err = err + 1;
	if (err>maxerr) goto exit;
	cout << "   error= " << err;
	exit:
	; //
}

//Richard
void getch()
{
	if (cc == ll)
	{
		if (diskfile.eof())
		{
			cout << "Program incomplete";
			goto exit;
		}
		ll = 0;
		cc = 0;
		cout << " ";
		while (ch != 10 || ch != 13)
		{
			ll = ll + 1;
			diskfile.get(ch);
			cout << ch;
			line[ll] = ch;
		}
		cout << endl;
		ll = ll + 1;
		diskfile.get(line[ll]);
	}
	cc = cc + 1;
	ch = line[cc];
exit:
	;
}

void getsym()
{
	int i, j, k;
	while (ch == ' ') getch();
	if (97 <= ch && ch <= 122)
	{
		k = 0;
		do
		{
			if (k < a1)
			{
				k = k + 1;
				a[k] = ch;
			}
			getch();
		} while ((48 <= ch && ch <= 57) || (97 <= ch && ch <= 122));

		if (k >= kk) kk = k;
		else
		{
			do
			{
				a[kk] = ' ';
				kk = kk - 1;
			} while (kk != k);

			for (i = 0; i<a1; i++) id[i] = a[i];
			i = 1;
			j = norw;
			do
			{
				k = (i + j) / 2;
				int ab = 0;
				if (id[ab] <= word[ab][k]) j = k - 1;
				if (id[ab] >= word[ab][k]) i = k + 1;

			} while (i <= j);
			if (i - 1 > j) sym = wsym[k];
			else sym = ident;
		}
	}
	else if (48 <= ch && ch <= 57)
	{
		k = 0;
		num = 0;
		sym = number;
		do
		{
			num = 10 * num + (ch - 48);
			k = k + 1;
			getch();
		} while (48 <= ch && ch <= 57);
		if (k > nmax) error(30);
	}
	else if (ch == ':')
	{
		getch();
		if (ch == '=')
		{
			sym = becomes;
			getch();
		}
		else sym = nul;
	}
	else if (ch == '<')
	{
		getch();
		if (ch = '=')
		{
			sym = leq;
			getch();
		}
		else sym = lss;
	}
	else if (ch == '>')
	{
		getch();
		if (ch == '=')
		{
			sym = geq;
			getch();
		}
		else sym = gtr;
	}
	else
	{
		sym = ssym[ch];
		getch();
	}
}

void gen(fct x, int y, int z)
{
	if (cx > cxmax)
	{
		cout << "Program too long";
		goto exit;
	}

	code[cx].f = x;
	code[cx].l = y;
	code[cx].a = z;
	cx += 1;
exit:
	;
}

//Edwin
void test(symset s1, symset s2, int n)
{
	if (!(nul <= sym && sym <= procsym))
	{
		error(n);
		//	s1 = s1+s2; //ga yakin neh

		while (!(nul <= sym && sym <= procsym))
		{
			getsym();
		} //yg ini jg ga yakin
	}
}

void enter(obj k)
{
	//enter object = (constant,variable,prozedure) into table
	tx = tx + 1;
	for (int ab = 0; ab<a1; ab++) table[tx].name[ab] = id[ab];
	table[tx].kind = k;
	switch (k)
	{
	case constant: if (num>amax)
	{
		error(30);
		num = 0;
	}
				   table[tx].val = num;
				   break;
	case variable: table[tx].level = lev;
		table[tx].adr = dx;
		dx = dx + 1;
		break;
	case prozedure: table[tx].level = lev;
		break;
	}
}

int position(alfa id)
{
	int i;
	//find identifier id in table
	for (i = 0; i<a1; i++) table[0].name[i] = id[i];
	i = tx;
	while (table[i].name != id) i = i - 1;
	return i;
}

void constdeclaration()
{
	if (sym == ident)
	{
		getsym();
		if (eql <= sym && sym <= becomes)
		{
			if (sym == becomes)
			{
				error(1); //gunakan = sebagai ganti :=
				getsym();
				if (sym == number)
				{
					enter(constant);
					getsym();
				}
				else error(2); //setelah = diikuti angka
			}
			else error(3); //setelah identifier diikuti =
		}
		else error(4); //setelah const, var, procedure diikuti identifier
	}
}

void vardeclaration()
{
	if (sym == ident)
	{
		enter(variable);
		getsym();
	}
	else error(4);//setelah const, var, procedire diikuti identifier
}

//Po Yuan
void listcode()
{
	int i;
	for (i = cx0; i<cx - 1; i++)
	{
		cout << i << endl;
		printf("     %c,   %d,     %c", mnemonic[code[i].f], code[i].l, code[i].a);
	}
}

void factor(symset fsys)
{
	int i;
	while (sym == ident || sym == number || sym == lparen)
	{
		if (sym == ident)
		{
			i = position(id);
			if (i<0)
				error(11);
			else
			{
				switch (table[i].kind)
				{
				case constant: gen(lit, 0, table[i].val); break;
				case variable: gen(lod, lev - table[i].level, table[i].adr); break;
				case prozedure: error(21); break;
				}
			}
		}
		else if (sym == number)
		{
			if (num>amax)
			{
				error(30);
				num = 0;
			}
			gen(lit, 0, num);
		}
		else if (sym == lparen)
		{
			getsym();
			fsys[testcount] = rparen;
			expression(fsys);
			testcount++;
			if (sym == rparen)
				getsym();
			else
				error(22);
		}
		//	test(fsys,rparen,23);
	}
}

void term(symset fsys)
{
	symbol mulop;
	fsys[testcount] = times;
	testcount++;
	fsys[testcount] = slash;
	testcount++;
	factor(fsys);
	while (sym == times || sym == slash)
	{
		mulop = sym;
		getsym();
		fsys[testcount] = times;
		testcount++;
		fsys[testcount] = slash;
		testcount++;
		factor(fsys);
		if (mulop == times)
			gen(opr, 0, 4);
		else
			gen(opr, 0, 5);
	}
}

void expression(symset fsys)
{
	symbol addop;
	if (sym == 3 || sym == 4)
	{
		addop = sym;
		getsym();
		fsys[testcount] = (symbol)3;
		testcount++;
		fsys[testcount] = (symbol)4;
		testcount++;
		term(fsys);
		if (addop == 4)
			gen(opr, 0, 1);
	}
	else
	{
		fsys[testcount] = (symbol)3;
		testcount++;
		fsys[testcount] = (symbol)4;
		testcount++;
		term(fsys);
	}
	while (sym == (symbol)3 || sym == (symbol)4)
	{
		addop = sym;
		getsym();
		fsys[testcount] = (symbol)3;
		testcount++;
		fsys[testcount] = (symbol)4;
		testcount++;
		term(fsys);
		if (addop = (symbol)3)
			gen(opr, 0, 2);
		else
			gen(opr, 0, 3);
	}
}

void condition(symset fsys)
{
	symbol relop;
	if (sym == oddsym)
	{
		getsym();
		expression(fsys);
		gen(opr, 0, 6);
	}
	else
	{
		fsys[testcount] = eql;
		testcount++;
		fsys[testcount] = neq;
		testcount++;
		fsys[testcount] = lss;
		testcount++;
		fsys[testcount] = gtr;
		testcount++;
		fsys[testcount] = leq;
		testcount++;
		fsys[testcount] = geq;
		testcount++;
		expression(fsys);
		if (sym != eql || sym != neq || sym != lss || sym != leq || sym != gtr || sym != geq)
			error(20);
		else
		{
			relop = sym;
			getsym();
			expression(fsys);
			switch (relop)
			{
			case eql:
				gen(opr, 0, 8);
				break;
			case neq:
				gen(opr, 0, 9);
				break;
			case lss:
				gen(opr, 0, 10);
				break;
			case geq:
				gen(opr, 0, 11);
				break;
			case gtr:
				gen(opr, 0, 12);
				break;
			case leq:
				gen(opr, 0, 13);
				break;
			}
		}
	}
}

bool infsys(symset fsys, symbol sym)
{
	for (int ab = 0; ab<testcount; ab++)
	{
		if (sym == fsys[ab]) return true;
		else return false;
	}
}

bool instat(symset stat, symbol sym)
{
	for (int ab = 0; ab<statcount; ab++)
	{
		if (sym == stat[ab]) return true;
		else return false;
	}
}

void statement(symset fsys)
{
	int i, cx1, cx2;
	fsys[testcount] = ident;
	testcount++;
	if (!infsys(fsys, sym))
	{
		do
		{
			getsym();
		} while (!infsys(fsys, sym));
	}

	if (sym == ident)
	{
		i = position(id);
		if (i<0) error(11);
		else if (table[i].kind == variable)
		{
			error(12);
			i = 0;
		}
		getsym();
		if (sym == becomes) getsym();
		expression(fsys);

		if (i != 0)
		{
			gen(sto, lev - table[i].level, table[i].adr);
		}
	}

	else if (sym == callsym)
	{
		getsym();
		if (sym != ident) error(14);
		else
		{
			i = position(id);
			if (i<0) error(11);
			else if (table[i].kind != prozedure)
				gen(cal, lev - table[i].level, table[i].adr);
			else
				error(15);
			getsym();
		}
	}

	else if (sym == ifsym)
	{
		getsym();
		fsys[testcount] = thensym;
		testcount++;
		fsys[testcount] = dosym;
		testcount++;
		condition(fsys);
		if (sym == thensym)
			getsym();
		else
			error(16);
		cx1 = cx;
		gen(jpc, 0, 0);
		statement(fsys);
		code[cx1].a = cx;
	}
	else if (sym == beginsym)
	{
		getsym();
		fsys[testcount] = semicolon;
		testcount++;
		fsys[testcount] = endsym;
		testcount++;
		statement(fsys);
		statbegsys[statcount] = semicolon;
		statcount++;
		while (instat(statbegsys, sym))
		{
			if (sym == semicolon)
				getsym();
			else
				error(10);
			fsys[testcount] = semicolon;
			testcount++;
			fsys[testcount] = endsym;
			testcount++;
			statement(fsys);
		}
		if (sym == endsym)
			getsym();
		else
			error(17);
	}
	else if (sym == whilesym)
	{
		cx1 = cx;
		getsym();
		fsys[testcount] = dosym;
		testcount++;
		condition(fsys);
		cx2 = cx;
		gen(jpc, 0, 0);
		if (sym == dosym)
			getsym();
		else
			error(18);
		statement(fsys);
		gen(jpc, 0, cx1);
		code[cx2].a = cx;
	}
	test(fsys, NULL, 19);
}


void block(int lev, int tx, symset fsys)
{
	int cx0, tx0;
	dx = 3, tx0 = tx;
	table[tx].adr = cx;
	gen(jmp, 0, 0);
	if (lev > levmax) error(30);
	do
	{
		if (sym == constsym)
		{
			getsym();
			do
			{
				constdeclaration();
				while (sym == comma)
				{
					getsym();
					constdeclaration();
				}
				if (sym == semicolon) getsym(); else error(5);
			} while (!(sym != ident));
		}

		if (sym == varsym)
		{
			getsym();
			do
			{
				vardeclaration();
				while (sym == comma)
				{
					getsym();
					vardeclaration();
				}
				if (sym == semicolon) getsym(); else error(5);
			} while (!(sym != ident));
		}

		while (sym == procsym)
		{
			getsym();
			if (sym == ident)
			{
				enter(prozedure);
				getsym();
			}
			else error(4);

			if (sym == semicolon) getsym(); else error(5);
			fsys[testcount] = semicolon;
			testcount++;
			block(lev + 1, tx, fsys);
			if (sym == semicolon)
			{
				getsym();
				statbegsys[statcount] = ident;
				statcount++;
				statbegsys[statcount] = procsym;
				statcount++;
				test(statbegsys, fsys, 6);
			}
		}
	} while (indeclbegsys(sym));
	code[table[tx0].adr].a = cx;
	table[tx0].adr = cx;
	table[tx0].size = dx;
	cx0 = cx;
	gen(intg, 0, dx);
	fsys[testcount] = semicolon;
	testcount++;
	fsys[testcount] = endsym;
	testcount++;
	statement(fsys);
	gen(opr, 0, 0);
	scanf_s("%c", &ch);
	listcode();
}

//Anastasia 

bool indeclbegsys(symbol sym)
{
	if ((sym == nul) || (sym == ident) || (sym == number) || (sym == 3) || (sym == 4) || (sym == times) || (sym == slash)
		|| (sym == oddsym) || (sym == eql) || (sym == neq) || (sym == lss) || (sym == leq) || (sym == gtr) || (sym == geq)
		|| (sym == lparen) || (sym == rparen) || (sym == comma) || (sym == semicolon) || (sym == period) || (sym == becomes)
		|| (sym == beginsym) || (sym == endsym) || (sym == ifsym) || (sym == thensym) || (sym == whilesym) || (sym == dosym)
		|| (sym == callsym) || (sym == constsym) || (sym == varsym) || (sym == procsym))
		return true;
	else return false;
}


int base(int l, int b)
{
	int b1;
	int s[500];

	b1 = b;
	while (l>0)
	{
		b1 = s[b1];
		l = l - 1;
	}
	return b1;
}

void interpret()
{
	const int stacksize = 500;
	int p = 0, b = 0, t = 0;
	instruction i;
	int s[stacksize];
	s[1] = 0; s[2] = 0; s[3] = 0;

	do
	{
		i = code[p];
		p = p + 1;
		switch (i.f)
		{
		case lit: t = t + 1; s[t] = i.a; break;
		case opr:
			switch (i.a)
			{
			case 0: t = b - 1; p = s[t + 3]; b = s[t + 2]; break;
			case 1: s[t] = -s[t]; break;
			case 2: t = t - 1; s[t] = s[t] + s[t + 1]; break;
			case 3: t = t - 1; s[t] = s[t] - s[t + 1]; break;
			case 4: t = t - 1; s[t] = s[t] * s[t + 1]; break;
			case 5: t = t - 1; s[t] = s[t] / s[t + 1]; break;
			case 6: s[t] = (int(odd(s[t]))); break;
			case 8: t = t - 1; s[t] = (int(s[t] = s[t + 1])); break;
			case 9: t = t - 1; s[t] = (int(s[t] != s[t + 1])); break;
			case 10: t = t - 1; s[t] = (int(s[t] < s[t + 1])); break;
			case 11: t = t - 1; s[t] = (int(s[t] >= s[t + 1])); break;
			case 12: t = t - 1; s[t] = (int(s[t] > s[t + 1])); break;
			case 13: t = t - 1; s[t] = (int(s[t] <= s[t + 1])); break;
			}
		case lod: t = t + 1; s[t] = s[base(1, b) + i.a]; break;
		case sto: s[base(1, b) + i.a] = s[t]; cout << s[t] << endl; t = t - 1; break;
		case cal: s[t + 1] = base(1, b); s[t + 2] = b; s[t + 3] = p; b = t + 1; p = i.a; break;
		case intg: t = t + i.a; break;
		case jmp: p = i.a; break;
		case jpc: if (s[t] = 0) { p = i.a; t = t - 1; } break;
		}
	} while (p != 0);
}

void insert2word(int index, char *str)
{
	int i;
	for (i = 0; i<10; i++) word[index][i] = str[i];
}

void main()
{
	int choice = 0;
	do
	{
		system("cls");
		printMenu();
		cin >> choice;
	} while (choice != 2);

	symset call; int i = 1;
	for (i = 1; i<a1; i++) { id[i] = ' '; a[i] = ' '; }
	for (i = 0; i<a1; i++) line[i] = ' ';
	for (i = 0; i<txmax; i++)
	{
		for (int j = 0; j<a1; j++) table[i].name[j] = id[j];
	}
	for (i = 0; i<norw; i++)
	{
		for (int j = 0; j<a1; j++)word[i][j] = ' ';
	}
	for (i = 0; i<norw; i++) { wsym[i] = nul; }
	for (ch = char(0); ch <= char(chsetsize - 1); ch++) ssym[ch] = nul;

	strcpy_s(word[0], "begin/Mulai     ");
	insert2word(1, "call      ");
	insert2word(2, "const/konst     "); insert2word(3, "do        ");
	insert2word(4, "end/akhir       "); insert2word(5, "if        ");
	insert2word(6, "odd       "); insert2word(7, "procedure/proc ");
	insert2word(8, "then      "); insert2word(9, "var      ");
	insert2word(10, "while     ");

	wsym[1] = beginsym;
	wsym[2] = callsym;
	wsym[3] = constsym;
	wsym[4] = dosym;
	wsym[5] = endsym;
	wsym[6] = ifsym;
	wsym[7] = oddsym;
	wsym[8] = procsym;
	wsym[9] = thensym;
	wsym[10] = varsym;
	wsym[11] = whilesym;

	ssym['+'] = (symbol)3;
	ssym['-'] = (symbol)4;
	ssym['*'] = times;
	ssym['/'] = slash;
	ssym['('] = lparen;
	ssym[')'] = rparen;
	ssym['='] = eql;
	ssym[','] = comma;
	ssym['.'] = period;
	ssym['#'] = neq;
	ssym['<'] = lss;
	ssym['>'] = gtr;
	ssym[';'] = semicolon;

	strcpy_s(mnemonic[lit], " LIT ");
	strcpy_s(mnemonic[opr], " OPR ");
	strcpy_s(mnemonic[lod], " LOD ");
	strcpy_s(mnemonic[sto], " STO ");
	strcpy_s(mnemonic[cal], " CAL ");
	strcpy_s(mnemonic[intg], " INTG ");
	strcpy_s(mnemonic[jmp], " JMP ");
	strcpy_s(mnemonic[jpc], " JPC ");

	declbegsys[0] = constsym; decount++;
	declbegsys[1] = varsym; decount++;
	declbegsys[2] = procsym; decount++;

	statbegsys[0] = beginsym; statcount++;
	statbegsys[1] = callsym; statcount++;
	statbegsys[2] = ifsym; statcount++;
	statbegsys[3] = whilesym; statcount++;

	facbegsys[0] = ident; facount++;
	facbegsys[1] = number; facount++;
	facbegsys[2] = lparen; facount++;

	system("cls");
	err = 0;
	cc = 0;
	cx = 0;
	ll = 0;
	ch = ' ';
	kk = a1;

	while (ch != 10 || ch != 13)
	{
		diskfile.get(ch);
		cout << ch;
	}
	int j = 0;
	do
	{
		getsym();
		call[j] = period; j++;
		for (i = 0; i<decount; i++)
		{
			call[j] = declbegsys[i];
			j++;
		}
		for (i = 0; i<statcount; i++)
		{
			call[j] = statbegsys[i];
			j++;
		}
		block(0, 0, call);
		if (err == 0) interpret();
		else cout << " kesalahan pada program OPAS" << endl;
	} while (sym != period);

	if (sym != period) error(9);
	diskfile.close();
}