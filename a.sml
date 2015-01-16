fun readfile fileToRead = let

  (****************************************************************************
  * Read the file into a character list and return the list.
  ****************************************************************************)
  fun getContents filename = let
    val ins = TextIO.openIn filename

    fun helper(elem: char option) =
      case elem of
           NONE       => (TextIO.closeIn ins; [])
        |  SOME(elem) => elem::helper(TextIO.input1 ins)
  in
    helper(TextIO.input1 ins)
  end

  (****************************************************************************
  * Take a character list and print each character to the screen.
  ****************************************************************************)
  fun showContents([]) = []
    | showContents(x::xs) = (print(Char.toString(x)); showContents xs)


  fun showLines([]) = []
    | showLines(x::xs) = (print x; showLines xs)

  (****************************************************************************
  * Assign the character list to elements.
  ****************************************************************************)
  val contents = String.implode(getContents fileToRead)
  val strings  = String.fields( (fn (c) => if c = #" " then true else false) contents) 
in
 map (fn (line) => print line) strings 
end;
