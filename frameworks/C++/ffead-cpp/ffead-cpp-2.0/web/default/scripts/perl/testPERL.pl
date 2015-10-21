if (!exists $_POST{'submit'}) {
print <<EOF
<html>
<head>
<title>Personal INFO</title>
</head>
<body>
<form method="post" action="testPERL.pl">
First Name:<input type="text" size="12" maxlength="12" name="Fname"><br/>
Last Name:<input type="text" size="12" maxlength="36" name="Lname"><br/>
Gender:<br/>
Male:<input type="radio" value="Male" name="gender"><br/>
Female:<input type="radio" value="Female" name="gender"><br/>
Please choose type of residence:<br/>
Steak:<input type="checkbox" value="Steak" name="food[]"><br/>
Pizza:<input type="checkbox" value="Pizza" name="food[]"><br/>
Chicken:<input type="checkbox" value="Chicken" name="food[]"><br/>
<textarea rows="5" cols="20" name="quote" wrap="physical">Enter your favorite quote!</textarea><br/>
Select a Level of Education:<br/>
<select name="education">
<option value="Jr.High">Jr.High</option>
<option value="HighSchool">HighSchool</option>
<option value="College">College</option></select><br/>
Select your favorite time of day:<br/>
<select name="TofD" size="3">
<option value="Morning">Morning</option>
<option value="Day">Day</option>
<option value="Night">Night</option></select><br/>
<input type="submit" value="submit" name="submit">
</form>
</body>
</html>
EOF
} else {
$Fname = $_POST{"Fname"};
$Lname = $_POST{"Lname"};
$gender = $_POST{"gender"};
$food = $_POST{"food"};
$quote = $_POST{"quote"};
$education = $_POST{"education"};
$TofD = $_POST{"TofD"};
$text = "Hello, ".$Fname." ".$Lname.".<br/>";
$text .= "You are ".$gender.", and you like ";
while (($key, $value) = each $food) {
  $text .= $value."<br/>";
}
$text .= "<i>".$quote."</i><br/>";
$text .= "You're favorite time is ".$TofD.", and you passed ".$education."!<br/>";
print <<EOF
<html>
<head>
<title>Personal INFO</title>
</head>
<body>
$text
</body>
</html>
EOF
}