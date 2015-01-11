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

  val insertOpen  = explode "insert into advisers_tmp (rawdata) values ('"
  val insertClose = explode "');"

  fun parse adviserList = 
  let
    fun processBlock([], closeToken) = []
	  | processBlock(y::ys, closeToken) = 
	    case y of
		  closeToken => processBlock([], closeToken)
		| _          => processBlock(ys, closeToken)

    fun helper([], eliminate, startRecord)    = []
	  | helper(x::xs, eliminate, startRecord) = 
	    case x of
          #"<"  => helper(xs, true, false)
		| #";"  => insertClose @ #"\n"::helper(xs, false, true)
        | _     => 
          if eliminate = true then helper(xs, true, false)
          else 
		    if (Char.isPrint x) = true then 
			  if startRecord = true then insertOpen @ x::helper(xs, false, false)
			  else x::helper(xs, false, false)
			else helper(xs, eliminate, false)
  in
    helper(adviserList, false, true)
  end


  (****************************************************************************
                            Values
  ****************************************************************************)
  val advisers = getcontents copythis
  val cleanedAdvisers = parse advisers
in
  putcontents(cleanedAdvisers, tothis)
end;

copyfile("Advisers.txt", "AdviserNames.txt");
