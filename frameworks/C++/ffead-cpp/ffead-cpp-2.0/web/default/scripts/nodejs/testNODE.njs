if(POST['submit'] == undefined)
{
  console.log('  \
    <html> \
    <head> \
    <title>Personal INFO</title> \
    </head> \
    <body> \
    <form method="post" action="testNODE.njs"> \
    First Name:<input type="text" size="12" maxlength="12" name="Fname"><br/> \
    Last Name:<input type="text" size="12" maxlength="36" name="Lname"><br/> \
    Gender:<br/> \
    Male:<input type="radio" value="Male" name="gender"><br/> \
    Female:<input type="radio" value="Female" name="gender"><br/> \
    Please choose type of residence:<br/> \
    Steak:<input type="checkbox" value="Steak" name="food[]"><br/> \
    Pizza:<input type="checkbox" value="Pizza" name="food[]"><br/> \
    Chicken:<input type="checkbox" value="Chicken" name="food[]"><br/> \
    <textarea rows="5" cols="20" name="quote" wrap="physical">Enter your favorite quote!</textarea><br/> \
    Select a Level of Education:<br/> \
    <select name="education"> \
    <option value="Jr.High">Jr.High</option> \
    <option value="HighSchool">HighSchool</option> \
    <option value="College">College</option></select><br/> \
    Select your favorite time of day:<br/> \
    <select name="TofD" size="3"> \
    <option value="Morning">Morning</option> \
    <option value="Day">Day</option> \
    <option value="Night">Night</option></select><br/> \
    <input type="submit" value="submit" name="submit"> \
    </form> \
    </body> \
    </html>');
}
else
{
  var text = "Hello, " + POST['Fname'] + " " +  POST['Lname'] + ".<br/>";
  text += "You are " + POST['gender'] + ", and you like ";
  for(var key in POST['food']) 
  {
    text += POST['food'][key] + "<br/>"
  }
  text +=  "<i>" + POST['quote'] + "</i><br/>";
  text +=  "You're favorite time is " + POST['TofD'] + ", and you passed " + POST['education'] + "!<br/>";
  console.log(' \
  <html> \
  <head> \
  <title>Personal INFO</title> \
  </head> \
  <body>' + text +
  '</body> \
  </html>');
}
