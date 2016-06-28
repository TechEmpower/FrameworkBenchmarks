
<html>
<head><title>${msg.auth.title}</title></head>

    <form action="http://localhost:8686/servlets/auth" method="POST" name="authenticationForm" id="authenticationForm">
	<br/>

        <table >
            <tr>
                  <td>
                  		${msg.auth.userName}
                  </td>
                  <td>
             			<input name="userName" id="userName" type="text"/>
                  </td>
            </tr>
            <tr>
                  <td>
                  		${msg.auth.password}
                  </td>
                  <td>
             			<input name="password" id="password" type="password"/>
                  </td>
            </tr>
            <tr>
                  <td>
                  </td>
                  <td>
                 	<input type="submit" value ="submit"/>
                  </td>
            </tr>
        </table>
    </form>

</html>
