// A Bison parser, made by GNU Bison 3.0.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2013 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


// First part of user declarations.
#line 21 "struct_parser.y" // lalr1.cc:399

#include "struct_parser.hh"

#define MY_ERR yy::CLASS_NAME::error

 // Globals
int j_buf = 0;
int i_undeclared = 0;

#line 46 "struct_parser.tab.cc" // lalr1.cc:399

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

#include "struct_parser.tab.hh"

// User implementation prologue.
#line 67 "struct_parser.y" // lalr1.cc:407

extern YY_DECL;

#line 63 "struct_parser.tab.cc" // lalr1.cc:407


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (/*CONSTCOND*/ false)
# endif


// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << std::endl;                  \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE(Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void>(0)
# define YY_STACK_PRINT()                static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyempty = true)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)


namespace yy {
#line 149 "struct_parser.tab.cc" // lalr1.cc:474

  /// Build a parser object.
  CLASS_NAME::CLASS_NAME (PARENT_STRUCT_TYPE &strct_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      strct (strct_yyarg)
  {}

  CLASS_NAME::~CLASS_NAME ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/

  inline
  CLASS_NAME::syntax_error::syntax_error (const location_type& l, const std::string& m)
    : std::runtime_error (m)
    , location (l)
  {}

  // basic_symbol.
  template <typename Base>
  inline
  CLASS_NAME::basic_symbol<Base>::basic_symbol ()
    : value ()
  {}

  template <typename Base>
  inline
  CLASS_NAME::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
    , location (other.location)
  {
    value = other.value;
  }


  template <typename Base>
  inline
  CLASS_NAME::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  inline
  CLASS_NAME::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const location_type& l)
    : Base (t)
    , value ()
    , location (l)
  {}

  template <typename Base>
  inline
  CLASS_NAME::basic_symbol<Base>::~basic_symbol ()
  {
  }

  template <typename Base>
  inline
  void
  CLASS_NAME::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move(s);
    value = s.value;
    location = s.location;
  }

  // by_type.
  inline
  CLASS_NAME::by_type::by_type ()
     : type (empty)
  {}

  inline
  CLASS_NAME::by_type::by_type (const by_type& other)
    : type (other.type)
  {}

  inline
  CLASS_NAME::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  CLASS_NAME::by_type::move (by_type& that)
  {
    type = that.type;
    that.type = empty;
  }

  inline
  int
  CLASS_NAME::by_type::type_get () const
  {
    return type;
  }


  // by_state.
  inline
  CLASS_NAME::by_state::by_state ()
    : state (empty)
  {}

  inline
  CLASS_NAME::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  inline
  void
  CLASS_NAME::by_state::move (by_state& that)
  {
    state = that.state;
    that.state = empty;
  }

  inline
  CLASS_NAME::by_state::by_state (state_type s)
    : state (s)
  {}

  inline
  CLASS_NAME::symbol_number_type
  CLASS_NAME::by_state::type_get () const
  {
    return state == empty ? 0 : yystos_[state];
  }

  inline
  CLASS_NAME::stack_symbol_type::stack_symbol_type ()
  {}


  inline
  CLASS_NAME::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s, that.location)
  {
    value = that.value;
    // that is emptied.
    that.type = empty;
  }

  inline
  CLASS_NAME::stack_symbol_type&
  CLASS_NAME::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    location = that.location;
    return *this;
  }


  template <typename Base>
  inline
  void
  CLASS_NAME::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    switch (yysym.type_get ())
    {
            case 3: // WORD

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.str)); }
#line 330 "struct_parser.tab.cc" // lalr1.cc:599
        break;

      case 5: // BOOL_VAR

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.bool_v)); }
#line 337 "struct_parser.tab.cc" // lalr1.cc:599
        break;

      case 6: // CHAR_VAR

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.char_v)); }
#line 344 "struct_parser.tab.cc" // lalr1.cc:599
        break;

      case 7: // INT_VAR

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.int_v)); }
#line 351 "struct_parser.tab.cc" // lalr1.cc:599
        break;

      case 8: // FLOAT_VAR

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.float_v)); }
#line 358 "struct_parser.tab.cc" // lalr1.cc:599
        break;

      case 22: // nums

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.vec)); }
#line 365 "struct_parser.tab.cc" // lalr1.cc:599
        break;

      case 23: // bools

#line 65 "struct_parser.y" // lalr1.cc:599
        { free ((yysym.value.vec)); }
#line 372 "struct_parser.tab.cc" // lalr1.cc:599
        break;


      default:
        break;
    }
  }

#if YYDEBUG
  template <typename Base>
  void
  CLASS_NAME::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " ("
        << yysym.location << ": ";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  inline
  void
  CLASS_NAME::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  inline
  void
  CLASS_NAME::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  inline
  void
  CLASS_NAME::yypop_ (unsigned int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  CLASS_NAME::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  CLASS_NAME::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  CLASS_NAME::debug_level_type
  CLASS_NAME::debug_level () const
  {
    return yydebug_;
  }

  void
  CLASS_NAME::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  inline CLASS_NAME::state_type
  CLASS_NAME::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  inline bool
  CLASS_NAME::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  CLASS_NAME::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  CLASS_NAME::parse ()
  {
    /// Whether yyla contains a lookahead.
    bool yyempty = true;

    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << std::endl;

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:

    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyempty)
      {
        YYCDEBUG << "Reading a token: ";
        try
          {
            yyla.type = yytranslate_ (yylex (&yyla.value, &yyla.location, strct));
          }
        catch (const syntax_error& yyexc)
          {
            error (yyexc);
            goto yyerrlab1;
          }
        yyempty = false;
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Discard the token being shifted.
    yyempty = true;

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;

      // Compute the default @$.
      {
        slice<stack_symbol_type, stack_type> slice (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, slice, yylen);
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
      try
        {
          switch (yyn)
            {
  case 4:
#line 79 "struct_parser.y" // lalr1.cc:847
    {
                                    try {
                                       (yystack_[3].value.bool_v)->set_slice((yystack_[2].value.spans)); (yystack_[3].value.bool_v)->read_buf((yystack_[0].value.vec));
                                       delete((yystack_[3].value.bool_v)); delete((yystack_[0].value.vec));
                                    } catch(const char *msg) {
                                       MY_ERR(yystack_[3].location, msg); YYABORT; }
                }
#line 620 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 5:
#line 86 "struct_parser.y" // lalr1.cc:847
    {
                                    try {
                                       (yystack_[3].value.char_v)->set_slice((yystack_[2].value.spans)); (yystack_[3].value.char_v)->read_buf((yystack_[0].value.vec));
                                       delete((yystack_[3].value.char_v)); delete((yystack_[0].value.vec));
                                    } catch(const char *msg) {
                                       MY_ERR(yystack_[3].location, msg); YYABORT; }
                }
#line 632 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 6:
#line 93 "struct_parser.y" // lalr1.cc:847
    {
                                    try {
                                       (yystack_[3].value.int_v)->set_slice((yystack_[2].value.spans)); (yystack_[3].value.int_v)->read_buf((yystack_[0].value.vec));
                                       delete((yystack_[3].value.int_v)); delete((yystack_[0].value.vec));
                                    } catch(const char *msg) {
                                       MY_ERR(yystack_[3].location, msg); YYABORT; }
                }
#line 644 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 7:
#line 100 "struct_parser.y" // lalr1.cc:847
    {
                                    try {
                                       (yystack_[3].value.float_v)->set_slice((yystack_[2].value.spans)); (yystack_[3].value.float_v)->read_buf((yystack_[0].value.vec));
                                       delete((yystack_[3].value.float_v)); delete((yystack_[0].value.vec));
                                    } catch(const char *msg) {
                                       MY_ERR(yystack_[3].location, msg); YYABORT; }
                }
#line 656 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 8:
#line 107 "struct_parser.y" // lalr1.cc:847
    { strcpy((yystack_[2].value.str_v), (yystack_[0].value.str)->c_str());
                                    delete((yystack_[0].value.str));
                }
#line 664 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 10:
#line 113 "struct_parser.y" // lalr1.cc:847
    {
                                    (yylhs.value.spans)[0][0] = (yystack_[3].value.span)[0]; (yylhs.value.spans)[0][1] = (yystack_[3].value.span)[1];
                                    (yylhs.value.spans)[1][0] = (yystack_[1].value.span)[0]; (yylhs.value.spans)[1][1] = (yystack_[1].value.span)[1];
                }
#line 673 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 11:
#line 117 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.spans)[0][0] = (yystack_[1].value.span)[0]; (yylhs.value.spans)[0][1] = (yystack_[1].value.span)[1];
                                    (yylhs.value.spans)[1][0] = 0;     (yylhs.value.spans)[1][1] = INT_MAX;
                }
#line 681 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 12:
#line 120 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.spans)[0][0] = 0;     (yylhs.value.spans)[0][1] = INT_MAX;
                                    (yylhs.value.spans)[1][0] = 0;     (yylhs.value.spans)[1][1] = INT_MAX;
                }
#line 689 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 13:
#line 125 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.span)[0] = (int)(yystack_[2].value.val) - 1; (yylhs.value.span)[1] = (int)(yystack_[0].value.val) - 1; }
#line 695 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 14:
#line 126 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.span)[0] = 0;           (yylhs.value.span)[1] = (int)(yystack_[0].value.val) - 1; }
#line 701 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 15:
#line 127 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.span)[0] = (int)(yystack_[1].value.val) - 1; (yylhs.value.span)[1] = INT_MAX; }
#line 707 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 16:
#line 128 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.span)[0] = (int)(yystack_[0].value.val) - 1; (yylhs.value.span)[1] = (yylhs.value.span)[0]; }
#line 713 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 17:
#line 129 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.span)[0] = 0;           (yylhs.value.span)[1] = INT_MAX; }
#line 719 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 18:
#line 130 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.span)[0] = 0;           (yylhs.value.span)[1] = INT_MAX; }
#line 725 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 19:
#line 133 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.vec) = (yystack_[2].value.vec); (yylhs.value.vec)->push_back(empty_vector);
                                    ++j_buf; (yylhs.value.vec)->at(j_buf).push_back((yystack_[0].value.val));
                }
#line 733 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 20:
#line 136 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.vec) = (yystack_[2].value.vec); (yylhs.value.vec)->at(j_buf).push_back((yystack_[0].value.val));
                }
#line 740 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 21:
#line 138 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.vec) = new Matrix (1, empty_vector);
                                    j_buf=0; (yylhs.value.vec)->at(j_buf).push_back((yystack_[0].value.val));
                }
#line 748 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 22:
#line 143 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.vec) = (yystack_[2].value.vec); (yylhs.value.vec)->push_back(empty_vector);
                                    ++j_buf; (yylhs.value.vec)->at(j_buf).push_back((bool)(yystack_[0].value.val));
                }
#line 756 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 23:
#line 146 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.vec) = (yystack_[2].value.vec); (yylhs.value.vec)->at(j_buf).push_back((bool)(yystack_[0].value.val));
                }
#line 763 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 24:
#line 148 "struct_parser.y" // lalr1.cc:847
    { (yylhs.value.vec) = new Matrix (1, empty_vector);
                                    j_buf=0; (yylhs.value.vec)->at(j_buf).push_back((bool)(yystack_[0].value.val));
                }
#line 771 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 25:
#line 152 "struct_parser.y" // lalr1.cc:847
    { switch ((int)(yystack_[0].value.val)) {
                                    case 0:   (yylhs.value.val) = 0.; break;
                                    case 1:   (yylhs.value.val) = 1.; break;
                                    default:  MY_ERR(yystack_[0].location, "Invalid boolean");
                                              YYABORT; }
                }
#line 782 "struct_parser.tab.cc" // lalr1.cc:847
    break;

  case 26:
#line 158 "struct_parser.y" // lalr1.cc:847
    { switch ((yystack_[0].value.str)->at(0)) {
                                    case 'F':
                                    case 'f':
                                    case 'N':
                                    case 'n': (yylhs.value.val) = 0.; break;
                                    case 'T':
                                    case 't':
                                    case 'Y':
                                    case 'y': (yylhs.value.val) = 1.; break;
                                    default:  MY_ERR(yystack_[0].location, "Invalid boolean");
                                              YYABORT; }
                                    delete((yystack_[0].value.str));
                }
#line 800 "struct_parser.tab.cc" // lalr1.cc:847
    break;


#line 804 "struct_parser.tab.cc" // lalr1.cc:847
            default:
              break;
            }
        }
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
    }
    goto yynewstate;

  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yyla.location, yysyntax_error_ (yystack_[0].state,
                                           yyempty ? yyempty_ : yyla.type_get ()));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyempty)
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyempty = true;
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    yyerror_range[1].location = yystack_[yylen - 1].location;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yyerror_range[1].location = yystack_[0].location;
          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyempty)
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (!yyempty)
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  void
  CLASS_NAME::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what());
  }

  // Generate an error message.
  std::string
  CLASS_NAME::yysyntax_error_ (state_type, symbol_number_type) const
  {
    return YY_("syntax error");
  }


  const signed char CLASS_NAME::yypact_ninf_ = -15;

  const signed char CLASS_NAME::yytable_ninf_ = -1;

  const signed char
  CLASS_NAME::yypact_[] =
  {
      -2,   -10,   -10,   -10,   -10,     3,     0,   -15,    -3,     5,
      17,    18,    19,    27,   -15,    -2,    20,    28,    -1,    14,
      29,    29,    29,   -15,   -15,    30,   -15,    -3,   -15,   -15,
     -15,     6,   -15,   -15,     8,     8,     8,   -15,    21,    14,
      14,    31,    33,   -15,   -15,   -15,   -15,   -15
  };

  const unsigned char
  CLASS_NAME::yydefact_[] =
  {
       9,    12,    12,    12,    12,     0,     0,     3,    18,     0,
       0,     0,     0,     0,     1,     9,    16,    17,     0,     0,
       0,     0,     0,     8,     2,    15,    14,    18,    11,    26,
      25,     4,    24,    21,     5,     6,     7,    13,     0,     0,
       0,     0,     0,    10,    23,    22,    20,    19
  };

  const signed char
  CLASS_NAME::yypgoto_[] =
  {
     -15,   -15,    23,     7,    12,     2,   -15,   -14
  };

  const signed char
  CLASS_NAME::yydefgoto_[] =
  {
      -1,     6,     7,     9,    18,    34,    31,    32
  };

  const unsigned char
  CLASS_NAME::yytable_[] =
  {
      14,    16,     8,     1,     2,     3,     4,     5,    17,    10,
      11,    12,    27,    13,    28,    19,    15,    29,    30,    39,
      40,    41,    42,    35,    36,    44,    45,    20,    21,    22,
      23,    25,    26,    33,    37,    46,    43,    47,    24,    38
  };

  const unsigned char
  CLASS_NAME::yycheck_[] =
  {
       0,     4,    12,     5,     6,     7,     8,     9,    11,     2,
       3,     4,    13,    10,    15,    10,    16,     3,     4,    13,
      14,    13,    14,    21,    22,    39,    40,    10,    10,    10,
       3,    11,     4,     4,     4,     4,    15,     4,    15,    27
  };

  const unsigned char
  CLASS_NAME::yystos_[] =
  {
       0,     5,     6,     7,     8,     9,    18,    19,    12,    20,
      20,    20,    20,    10,     0,    16,     4,    11,    21,    10,
      10,    10,    10,     3,    19,    11,     4,    13,    15,     3,
       4,    23,    24,     4,    22,    22,    22,     4,    21,    13,
      14,    13,    14,    15,    24,    24,     4,     4
  };

  const unsigned char
  CLASS_NAME::yyr1_[] =
  {
       0,    17,    18,    18,    19,    19,    19,    19,    19,    19,
      20,    20,    20,    21,    21,    21,    21,    21,    21,    22,
      22,    22,    23,    23,    23,    24,    24
  };

  const unsigned char
  CLASS_NAME::yyr2_[] =
  {
       0,     2,     3,     1,     4,     4,     4,     4,     3,     0,
       5,     3,     0,     3,     2,     2,     1,     1,     0,     3,
       3,     1,     3,     3,     1,     1,     1
  };


#if YYDEBUG
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const CLASS_NAME::yytname_[] =
  {
  "$end", "error", "$undefined", "WORD", "NUM", "BOOL_VAR", "CHAR_VAR",
  "INT_VAR", "FLOAT_VAR", "STR_VAR", "EQ", "COLON", "OPENB", "COMMA",
  "SEMI", "CLOSEB", "ENDL", "$accept", "file", "line", "dims", "dim",
  "nums", "bools", "bool", YY_NULLPTR
  };


  const unsigned char
  CLASS_NAME::yyrline_[] =
  {
       0,    75,    75,    76,    79,    86,    93,   100,   107,   110,
     113,   117,   120,   125,   126,   127,   128,   129,   130,   133,
     136,   138,   143,   146,   148,   152,   158
  };

  // Print the state stack on the debug stream.
  void
  CLASS_NAME::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  CLASS_NAME::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):" << std::endl;
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  // Symbol number corresponding to token number t.
  inline
  CLASS_NAME::token_number_type
  CLASS_NAME::yytranslate_ (int t)
  {
    static
    const token_number_type
    translate_table[] =
    {
     0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16
    };
    const unsigned int user_token_number_max_ = 271;
    const token_number_type undef_token_ = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned int> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }


} // yy
#line 1153 "struct_parser.tab.cc" // lalr1.cc:1155
#line 173 "struct_parser.y" // lalr1.cc:1156


void MY_ERR(yy::location const &loc, const std::string &s) {
    std::cerr << "parse_driver) ERROR at L" << loc.begin.line << ") " << s
              << std::endl;
}
