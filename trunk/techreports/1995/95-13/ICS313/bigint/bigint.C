#include <iostream.h>
Class "Bigint"
class BigInt 
/* 
 BigInt class. Each instance of this class represents a big
 integer. 
*/
{
 private:
    char* digits;// internal representation of bigint is a string
               // of binary-coded decimal (BCD) digits with
               // least significant digit first.
               // For example, 485 is stored as 5 8 4.
    int ndigits; //number of digits
 public:
    BigInt (const char* digitString);//convert digitStr->bigint
    BigInt (int n);                  //convert int -> bigint
    BigInt (const BigInt& n);        //copy bigint
    BigInt (char* d, int n);         //create bigint using digits
                                     //d and length of n
    BigInt operator+ (const BigInt& n); //add two bigint
    void print();                     //print bigint
    ~BigInt();                        //delete bigint 
    friend class DigitStream; //allow to access private members
                             //of bigint. 

};

Class "DigitStream"
class DigitStream 
/* 
   DigitStream Class. This class provides operations to scan the 
   internal representation of BigInt. Each instance of this class
   holds a pointer to current digit of the corresponding BigInt 
   object, and the number of digits.
 */
{
 private:
    char* dp;  //pointer to current digit being scanned
    int nd;    //number of digits
 public:
  DigitStream (const BigInt& );
  int operator++();
};


Function "Bigint::BigInt (int n)"
Bigint :: BigInt(int n)
/*
  Convert integer n into internal representation of bigint
*/
{
  char d[3*sizeof(int)+1];	// buffer for decimal digits
                                // 3*sizeof(int) = 3*4 = 12 
                                // max digits for an int is 12 on a
                                // Sun workstation
  char* dp = d;	                // pointer to next decimal digit

  do 
    {
      *++dp = n%10;		// integer and char are mixed.
      n /= 10;
      ndigits++;
    } while (n >= 0);
  digits = new char[ndigits];	
  register int i;
  for (i=0; i<=ndigits; i++) digits[i] = d[i];
}

Function "Bigint::BigInt (const BigInt& n)"
BigInt :: BigInt (const BigInt& n)
/* Copy constructor */
{
  int i;
  digits = new char[ndigits=i];
  char* p = digits;
  char* q = n.digits;
  while (--i) *p++ = *q++;
}

Function "Bigint::BigInt (char* d, int n)"
BigInt::BigInt(char* d, int n)
/* 
  Create instance of bigint using character digits 
  in d of size n 
*/

{
  digits = d;
  ndigits = n;
}

Function "Bigint::BigInt (const char* digitString)"
BigInt::BigInt (const char* digitString)
/*
  Convert character string digitString into internal 
  representation of bigint
*/
{
  int n=strlen(digitString);
  digits = new char [ndigits = n];
  char* p = digits;
  const char* q = &digitString[n];
  while(n--)
    *p++ = *q-- - '0';
}

Function "Bigint::~BigInt"
BigInt::~BigInt()
/* Destructor */
{
  delete digits;
}

Function "DigitStream::DigitStream"
DigitStream::DigitStream(const BigInt& n) {
  dp = n.digits;
  nd = n.ndigits;
}

Function "DigitStream::operator++"
int DigitStream::operator++() 
/* 
   Returns first digit in the digit stream and advance
   the digit stream by one
*/
{
    nd--;           //decrement number of digits
    return *dp++;
}

Function "Bigint::operator+"
BigInt BigInt::operator+ (const BigInt& n)
/* Add two BigInt */
{
  //maxDigits = max(n1,n2)+1 (add one extra digit for carry)
  int maxDigits=(ndigits>n.ndigits ? ndigits: n.ndigits) + 1;
  char* sumPtr = new char[maxDigits];
  BigInt sum(sumPtr,maxDigits);
  DigitStream a(*this);		// left operand
  DigitStream b(n);
  
  int i;
  int carry=0;
  while(i--){
    *sumPtr = (a++) + (b++) + carry;
    if(*sumPtr>9){
      carry=1;
      *sumPtr-=10;
    }
    else carry=0;
  }
  return sum;
}

Function "Bigint::print"
void BigInt::print()
/* Print the big integer */
{
  int i;
  for (i=ndigits;i>0 ;i--)
    cout << (int) digits[i];
  cout << "\n";
}


Function "main"
main()
{
  BigInt a = "123456";
  BigInt b = "200004";
  BigInt c = a + b;
  c.print();
}

