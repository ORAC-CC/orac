FUNCTION FMT, t, u
   COMPILE_OPT HIDDEN, LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS
   ;; Format a title and unit for printing as a plot title
   if KEYWORD_SET(u) $
   then RETURN, '!15'+t+'!13 ['+u+']!X'  $
   else RETURN, '!15'+t+'!X'
END
