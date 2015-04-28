app.controller("MainController", function($scope,  $http){
    $scope.understand = "I now understand how the scope works!";
    $scope.inputValue = "Hi Sanjoy !";

    $scope.selectedPerson = 0;
    $scope.selectedGenre = null;
    $scope.people = [
		{
			id: 0,
			name: 'Leon',
			music: [
				'Rock',
				'Metal',
				'Dubstep',
				'Electro'
			]
		},
		{
			id: 1,
			name: 'Chris',
			music: [
				'Indie',
				'Drumstep',
				'Dubstep',
				'Electro'
			]
		},
		{
			id: 2,
			name: 'Harry',
			music: [
				'Rock',
				'Metal',
				'Thrash Metal',
				'Heavy Metal'
			]
		},
		{
			id: 3,
			name: 'Allyce',
			music: [
				'Pop',
				'RnB',
				'Hip Hop'
			]
		}
	];

                $scope.errors = [];
                $scope.msgs = [];

	$scope.SignUp = function() {
 
                    $scope.errors.splice(0, $scope.errors.length); // remove all error messages
                    $scope.msgs.splice(0, $scope.msgs.length);
 
                    $http.post('http://localhost/service/tracywith/home/angular_js', {'username': $scope.username, 'userpassword': $scope.userpassword, 'useremail': $scope.useremail}
                    ).success(function(data, status, headers, config) {
 			
                        if (data == '1') {
                            $scope.msgs.push("Thanks! You have Registered Successfully!");
                        } else {
                            $scope.errors.push("Error Occured while saving into Database");
                        }
                    }).error(function(data, status) { // called asynchronously if an error occurs
                        // or server returns response with an error status.
                        $scope.errors.push(status);
                    });
                }


});
