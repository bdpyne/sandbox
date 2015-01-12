declare
  cursor c is select * from advisers_tmp;  
  type ctab is table of c%rowtype;
  adv   ctab;
  
  firstname  string(100);
  lastname  string(100);
  
  procedure parsename(str String, fname out string, lname out string)
  is
    pos     integer := 0;
    temp   string(100);
    idx      integer := 1;
  begin
      pos := instr(str, ' ', idx, 1);
      fname := substr(str, idx, pos - idx);
      idx    := pos + 1;
      pos := instr(str, ' ', idx, 1);
      if pos = 0 then
        lname := substr(str, idx, length(str) - (idx - 1));
      else            
        idx    := pos + 1;
        lname := substr(str, idx, length(str) - (idx - 1));        
      end if;
  end;
  
begin
    open c;
    fetch c bulk collect into adv;
    close c;
    
    for i in adv.first..adv.last loop
      parsename(adv(i).rawval, firstname, lastname);
      update advisers_tmp
      set fname = firstname, lname = lastname
      where rawval = adv(i).rawval;       
    end loop;
    commit;
end;
/