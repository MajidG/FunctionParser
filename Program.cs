using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Reflection;

namespace FunctionParser
{
    public enum NodeType
    {
        Variable,
        Constant,
        Operator
    }

    internal enum TokenType
    {
        None,
        Identifier,
        Value,
        Symbol
    }

    internal struct Token
    {
        public string Text;
        public TokenType type;
    }

    public static class PostfixTree
    {
        private static MethodInfo Cos = typeof(Math).GetMethod("Cos");
        private static Token CurrentToken;
        private static ConstantExpression E = Expression.Constant(Math.E);
        private static string Exp;
        private static int index = 0;
        private static ConstantExpression PI = Expression.Constant(Math.PI);

        private static Token PrevToken;

        private static MethodInfo Sin = typeof(Math).GetMethod("Sin");

        private static ParameterExpression X = Expression.Parameter(typeof(double), "x");

        public static Expression Build(Node node)
        {
            switch (node.Type)
            {
                case NodeType.Variable:
                    if (node.Text == "x")
                        return X;
                    if (node.Text == "pi")
                        return PI;
                    if (node.Text == "e")
                        return E;
                    throw new Exception();

                case NodeType.Constant:
                    double cnt = double.Parse(node.Text);
                    return Expression.Constant(cnt);

                case NodeType.Operator:
                    switch (node.Text[0])
                    {
                        case ':':
                            var param = Build(node.Right);
                            MethodInfo meth = null;
                            if (node.Left.Text == "sin")
                                meth = Sin;
                            if (node.Left.Text == "cos")
                                meth = Cos;
                            if (meth != null)
                                return Expression.Call(meth, param);
                            throw new Exception();
                        case '^':
                            var left = Build(node.Left);
                            var right = Build(node.Right);
                            return Expression.Power(left, right);

                        case '*':
                            left = Build(node.Left);
                            right = Build(node.Right);
                            return Expression.Multiply(left, right);

                        case '/':
                            left = Build(node.Left);
                            right = Build(node.Right);
                            return Expression.Divide(left, right);

                        case '+':
                            left = Build(node.Left);
                            right = Build(node.Right);
                            return Expression.Add(left, right);

                        case '-':
                            left = Build(node.Left);
                            right = Build(node.Right);
                            return Expression.Subtract(left, right);

                        default:
                            break;
                    }
                    break;

                default:
                    break;
            }
            throw new Exception();
        }

        public static Func<double, double> Compile(string exp)
        {
            Node node = Parse(exp);
            Expression body = Build(node);
            Expression<Func<double, double>> expression = Expression.Lambda<Func<double, double>>(body, new ParameterExpression[] { X });
            return expression.Compile();
        }

        public static void PrintTree(Node node, int index = 0)
        {
            if (node == null)
                return;
            Console.WriteLine("{0," + index + "}", node.Text);
            PrintTree(node.Left, index + 5);
            PrintTree(node.Right, index + 5);
        }

        private static bool isOp(char op)
        {
            switch (op)
            {
                case '=':
                case '+':
                case '-':
                case '*':
                case '/':
                case '^':
                case '(':
                case ')':
                    return true;

                default:
                    return false;
            }
        }

        private static bool Next()
        {
            if (index < Exp.Length && char.IsWhiteSpace(Exp[index]))
                index++;
            if (index >= Exp.Length)
                return false;

            string temp = "";
            if (char.IsLetter(Exp[index]))
            {
                temp += Exp[index];
                index++;
                while (index < Exp.Length && char.IsLetterOrDigit(Exp[index]))
                {
                    temp += Exp[index];
                    index++;
                }
                CurrentToken = new Token() { type = TokenType.Identifier, Text = temp };
                return true;
            }
            if (char.IsDigit(Exp[index]))
            {
                temp += Exp[index];
                index++;
                while (index < Exp.Length && char.IsDigit(Exp[index]))
                {
                    temp += Exp[index];
                    index++;
                }
                CurrentToken = new Token() { type = TokenType.Value, Text = temp };
                return true;
            }
            if (isOp(Exp[index]))
            {
                temp += Exp[index];
                index++;
                CurrentToken = new Token() { type = TokenType.Symbol, Text = temp };
                return true;
            }
            return false;
        }

        private static int op_prec(char op)
        {
            switch (op)
            {
                case '=':
                    return 0;

                case '+':
                case '-':
                    return 1;

                case '*':
                case '/':
                    return 2;

                case '^':
                    return 3;

                case ':':
                    return 4;

                default:
                    break;
            }
            return 0;
        }

        private static Node Parse(string exp)
        {
            try
            {
                index = 0;
                Exp = exp;
                Queue<Node> nodes = new Queue<Node>();
                Stack<char> ops = new Stack<char>();
                while (Next())
                {
                    switch (CurrentToken.type)
                    {
                        case TokenType.Identifier:
                            nodes.Enqueue(new Node { Type = NodeType.Variable, Text = CurrentToken.Text });
                            break;

                        case TokenType.Value:
                            nodes.Enqueue(new Node { Type = NodeType.Constant, Text = CurrentToken.Text });
                            break;

                        case TokenType.Symbol:
                            if (CurrentToken.Text[0] == '(')
                            {
                                switch (PrevToken.type)
                                {
                                    case TokenType.Identifier:
                                        ops.Push(':');
                                        break;

                                    case TokenType.Value:
                                        throw new Exception();
                                        break;
                                }
                                ops.Push(CurrentToken.Text[0]);
                            }
                            else if (CurrentToken.Text[0] == ')')
                            {
                                while (ops.Peek() != '(')
                                {
                                    nodes.Enqueue(new Node { Type = NodeType.Operator, Text = ops.Pop().ToString() });
                                }
                                ops.Pop();
                            }
                            else
                            {
                                while (ops.Count > 0 && op_prec(ops.Peek()) >= op_prec(CurrentToken.Text[0]))
                                {
                                    nodes.Enqueue(new Node { Type = NodeType.Operator, Text = ops.Pop().ToString() });
                                }
                                ops.Push(CurrentToken.Text[0]);
                            }
                            break;
                    }
                    PrevToken = CurrentToken;
                }
                if (ops.Contains('('))
                    throw new Exception();
                while (ops.Count > 0)
                    nodes.Enqueue(new Node { Type = NodeType.Operator, Text = ops.Pop().ToString() });

                Stack<Node> eval = new Stack<Node>();
                while (nodes.Count > 0)
                {
                    Node node = nodes.Dequeue();
                    if (node.Type == NodeType.Operator)
                    {
                        node.Right = eval.Pop();
                        node.Left = eval.Pop();
                    }
                    eval.Push(node);
                }
                if (eval.Count == 1)
                    return eval.Pop();
                else
                    throw new Exception();
            }
            catch
            {
                return null;
            }
        }
    }

    public class Node
    {
        public Node Left { get; set; }
        public Node Right { get; set; }
        public string Text { get; set; }
        public NodeType Type { get; set; }
    }

    internal class Program
    {
        private static void Main(string[] args)
        {
            try
            {
            while (true)
            {
                Console.Write("****\nf(x)=");
                string exp = Console.ReadLine();// "cos(x)";// x in radians
                var func = PostfixTree.Compile(exp);
                Console.Write("x = ");
                double x0 = double.Parse(Console.ReadLine());
                double result = func(x0);
                Console.WriteLine("####\nf(x) = " + exp);
                Console.WriteLine("f({0}) = {1}", x0, result);
            }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }  
        }
    }
}