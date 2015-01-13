fun copyfile(copythis, tothis) =
let
  (****************************************************************************
                            Functions
  ****************************************************************************)
  fun getcontents filename =
  let
    val ins  = TextIO.openIn filename

    fun helper(elem: char option) = 
      case elem of
           NONE       => (TextIO.closeIn ins; [])
         | SOME(elem) => elem::helper(TextIO.input1 ins)
  in
    helper(TextIO.input1 ins) 
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

  val insertOpen  = explode "insert into advisers_tmp (rawval) values ('"
  val insertClose = explode "');"

  fun parse rawlist = 
  let
    fun helper([], skip)    = []
     |  helper(x::xs, skip) =
	      case x of
		    #"<"  => insertOpen @ helper(xs, false)
                  | #">"  => insertClose @ #"\n"::helper(xs, true) 
		  | _     => 
		    if skip = true then helper(xs, skip)
		    else x::helper(xs, skip)
  in
    helper(rawlist, true)
  end


  (****************************************************************************
                            Values
  ****************************************************************************)
  val advisers = getcontents copythis
  val cleanedAdvisers = parse advisers
in
  putcontents(cleanedAdvisers, tothis)
end;

copyfile("Advisers.txt", "AdviserNames.sql");
