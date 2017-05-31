unit MathExpressionParser;

interface
Uses
  Sysutils, Classes, Math, Generics.Collections;

type
  TAssociativity = (asNone,asLeft, asRight);
  TOperatorRec = record
    iOperator: char;
    Precedence: integer;
    associativity: TAssociativity;
  end;
  //Expression parsing using the Shunting-yard algorithm
  TExpressionParser = class
  private
    FFormat: TFormatSettings;
    FDecimalSeperator: Char;
    FMaxDec: integer;
    function RemoveSqrt(S: string; os: integer=1): string;
    function RemoveSqr(s: string; os: integer=1): string;
    function CreateRPN(Exp: string): string;
    function ProcessRPN(rpn: string): double;
    procedure SetDecimalSeperator(const Value: Char);
    function GetDecFormat:string;
  public
    constructor Create(ADecimalSeperator: Char = '.'; AMaxDecimals: integer = 2);
    function ParseExpressionToString(Expression: string): string;
    function ParseExpressionToFloat(Expression: string): double;
    property DecimalSeperator: Char read FDecimalSeperator write SetDecimalSeperator;
    property MaxDecimals: integer read FMaxDec write FMaxDec;
  end;

  const
    scOperators = '^*/+-()';
    MaxOperators = 6;
    FloatForm = '####################.%s';
    Operators: array [0..MaxOperators] of TOperatorRec = (
      (iOperator: '^'; Precedence: 4; associativity: asRight),
      (iOperator: '*'; Precedence: 3; associativity: asLeft),
      (iOperator: '/'; Precedence: 3; associativity: asLeft),
      (iOperator: '+'; Precedence: 2; associativity: asLeft),
      (iOperator: '-'; Precedence: 2; associativity: asLeft),
      (iOperator: '('; Precedence: 1; associativity: asNone),
      (iOperator: ')'; Precedence: 1; associativity: asNone)
    );

implementation

{ TExpressionParser }
procedure TExpressionParser.SetDecimalSeperator(const Value: Char);
begin
  FDecimalSeperator := Value;
  FFormat.DecimalSeparator := FDecimalSeperator;
end;

//Calculating square roots in advance.
function TExpressionParser.RemoveSqrt(S: string; os: integer =1): string;
var
  J: integer;
  C: Char;
  LSqrtPos,
  LPosOpenBracket, LPosCloseBracket,
  LDepth: integer;
  LNumber,
  LSqrtString: string;
begin
  S := UpperCase(S);
  LSqrtPos := Pos('SQRT',S, os);

  if (LSqrtPos = 0) or (LSqrtPos+1 > Length(S)) then
    Exit(S);

  LPosOpenBracket := Pos('(',S,LSqrtPos);
  LDepth := 1;
  J := LPosOpenBracket;

  repeat
    J := J+1;
    if S[J] = '(' then Inc(LDepth)
    else if S[J] = ')' then Dec(LDepth);
  until LDepth = 0;

  LPosCloseBracket := J;

  LNumber := Copy(S, LPosOpenBracket+1, (LPosCloseBracket-LPosOpenBracket)-1);
  LSqrtString := Format('SQRT(%s)', [LNumber]);

  for C in LNumber do
    if Pos(C, scOperators)>0 then
      LNumber := ParseExpressionToString(LNumber);

  S := StringReplace(S,
                     LSqrtString,
                     Format('%s',[
                                  FormatFloat(
                                    Format(FloatForm,[GetDecFormat]),
                                           Sqrt( StrToInt( LNumber ) ) )
                                    ]
                           ),[]);
  Result := RemoveSqrt(S, LSqrtPos+1);
end;

//Calculating squares in advance.
function TExpressionParser.RemoveSqr(s: string; os: integer=1): string;
var
  J: integer;
  C: Char;
  LSqrPos,
  LPosOpenBracket, LPosCloseBracket,
  LDepth: integer;
  LNumber,
  LSqrString: string;
begin
  S := UpperCase(S);
  LSqrPos := Pos('SQR',S, os);

  if (LSqrPos = 0) or (LSqrPos+1 > Length(S)) then
    Exit(S);

  LPosOpenBracket := Pos('(',S,LSqrPos);
  LDepth := 1;
  J := LPosOpenBracket;

  repeat
    J := J+1;
    if S[J] = '(' then Inc(LDepth)
    else if S[J] = ')' then Dec(LDepth);
  until LDepth = 0;

  LPosCloseBracket := J;

  LNumber := Copy(S, LPosOpenBracket+1, (LPosCloseBracket-LPosOpenBracket)-1);
  LSqrString := Format('SQR(%s)', [LNumber]);

  for C in LNumber do
    if Pos(C, scOperators)>0 then
      LNumber := ParseExpressionToString(LNumber);

  S := StringReplace(S,
                     LSqrString,
                     Format('%s',[
                                  FormatFloat(
                                    Format(FloatForm,[GetDecFormat]),
                                           Sqr( StrToInt( LNumber ) ) )
                                    ]
                           ),[]);
  Result := RemoveSqr(S, LSqrPos+1);
end;

constructor TExpressionParser.Create(ADecimalSeperator: Char; AMaxDecimals: integer);
begin
  FDecimalSeperator := ADecimalSeperator;
  FFormat := TFormatSettings.Create;
  FFormat.DecimalSeparator := FDecimalSeperator;
  FMaxDec := AMaxDecimals;
end;

function TExpressionParser.CreateRPN(Exp: string): string;

  function FindOperator(Ch: Char): TOperatorRec;
  var
    I: integer;
  begin
    for I := 0 to MaxOperators do
      if Operators[I].iOperator = Ch then
        exit(Operators[I]);
    raise Exception.Create('Invalid operator!');
  end;

var
  LOperators: TStack<TOperatorRec>;
  LNumberStack: TStack<string>;
  LNumber: string;
  C: char;
  LCurrentOperator: TOperatorRec;
begin
  Result := '';
  LOperators := TStack<TOperatorRec>.Create;
  try
    LNumberStack := TStack<string>.Create;
    try
      Exp := RemoveSqr(
               RemoveSqrt(
                 StringReplace(
                   StringReplace(Exp, ' ', '', [rfReplaceAll]),
                 '--', '+', [rfReplaceAll])
               )
             );

      for C in Exp do
        if (CharInSet(C, ['0'..'9'])or (C='.')) then
          LNumber := LNumber + C
        else
        begin
          if LNumber <> '' then
            LNumberStack.Push(LNumber);
          LNumber := '';

          LCurrentOperator := FindOperator(C);
          if LCurrentOperator.iOperator = ')' then
          begin
            LNumberStack.Push(LOperators.Pop.iOperator);
            LOperators.Pop;
          end
          else if LCurrentOperator.iOperator = '(' then
          begin
            LOperators.Push(LCurrentOperator);
          end
          else if LCurrentOperator.iOperator = '^' then
            LOperators.Push(LCurrentOperator)
          else if LOperators.Count > 0 then
          begin
            if (LOperators.Peek.Precedence >= LCurrentOperator.Precedence) then
            begin
              LNumberStack.Push(LOperators.Pop.iOperator);
              LOperators.Push(LCurrentOperator);
            end
            else
              LOperators.Push(LCurrentOperator);
          end
          else
            LOperators.Push(LCurrentOperator);
        end;
        if LNumber <> '' then
          LNumberStack.Push(LNumber);
        while LOperators.Count > 0 do
          LNumberStack.Push(LOperators.Pop.iOperator);
        while LNumberStack.Count > 0 do
          Result := Format('%s %s', [LNumberStack.Pop, Result]);
    finally
      LNumberStack.Free
    end;
  finally
    LOperators.Free
  end;

end;

function TExpressionParser.GetDecFormat: string;
begin
  Result := StringOfChar('#', FMaxDec)
end;

function TExpressionParser.ParseExpressionToFloat(Expression: string): double;
var
  //Reversed polish notation
  RevPolNotation: string;
begin
  RevPolNotation := CreateRPN(Expression);
  Result := ProcessRPN(RevPolNotation);
end;

function TExpressionParser.ParseExpressionToString(Expression: string): string;
begin
  Result := FormatFloat(Format(FloatForm,[GetDecFormat]),ParseExpressionToFloat(Expression))
end;

function TExpressionParser.ProcessRPN(rpn: string): double;
var
  LStack: TStack<double>;
  LRpnList: TStringList;
  I: Integer;
  procedure DoCalculation(ACode: integer);
  var
    A,B: double;
  begin
    A := LStack.Pop;
    B := LStack.Pop;
    case ACode of
      42: LStack.Push(B * A);// Multiply
      43: LStack.Push(B + A);// Add
      45: LStack.Push(B - A);// Substract
      47: LStack.Push(B / A);// Divide
      94: LStack.Push(Power(B, A));// Power
      else
      begin
        LStack.Push(B);
        LStack.Push(A);
      end;
    end;
  end;
begin
  LStack := TStack<double>.Create;
  try
    LRpnList := TStringList.Create;
    LRpnList.Delimiter := ';';
    LRpnList.StrictDelimiter := True;
    LRpnList.DelimitedText := StringReplace(rpn, ' ', ';',[rfReplaceAll]);
    LStack.Push(StrToFloat(LRpnList[0], FFormat));
    for I := 1 to LRpnList.Count-1 do
      if LRpnList[I] = '' then
        continue
      else if not CharInSet(LRpnList[I][1], ['0'..'9']) then
        DoCalculation(Ord(LRpnList[I][1]))
      else
        LStack.Push(StrToFloat(LRpnList[I], FFormat));
    Result := LStack.Pop;
  finally
    LStack.Free
  end
end;

end.
