fun copyfile(copythis, tothis) =
let
  fun getcontents filename =
  let
    val ins  = TextIO.openIn filename

    fun helper(elem: char option, eliminate: bool) = 
      case elem of
           NONE       => (TextIO.closeIn ins; [])
         | SOME(elem) =>
             if elem = #">" then
               #"\n"::helper((TextIO.input1 ins), false)
             else
               if elem = #"<" then
                 helper((TextIO.input1 ins), true)
               else 
                 if elem = #"\n" then
                   helper((TextIO.input1 ins), eliminate)
                 else
                   if eliminate = true then
                     helper((TextIO.input1 ins), true)
                   else
                     elem::helper((TextIO.input1 ins), false)
  in
    helper((TextIO.input1 ins), false) 
  end

  fun putcontents(contents, filename) = 
  let
    val outs = TextIO.openOut filename

    fun helper([])    = TextIO.closeOut outs
      | helper(x::xs) = (TextIO.output1(outs, x);
                           helper(xs))
  in
    helper contents
  end

  fun toStringList charlist = implode charlist;
in
  putcontents((getcontents copythis), tothis)
(*  String.tokens (fn: x => if x = #"\n" then true else false) (implode (getcontents copythis)) *)
end;

copyfile("AdviserList.txt", "AdviserNames.txt");
