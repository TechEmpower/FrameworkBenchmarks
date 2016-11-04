#!/usr/bin/python
#
# Zips the results file and emails it to a specified address.
#
# @author A. Shawn Bandy
#
import smtplib
from os.path import basename
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from email import encoders
from email.utils import COMMASPACE, formatdate
import zipfile
import os
import subprocess
import tempfile

#
# finds a file of the given name in the tree below path
# http://stackoverflow.com/a/1724723
#
def find(name, path):
    for root, dirs, files in os.walk(path):
        if name in files:
            return os.path.join(root, name)

#
# Generate body of email and attach to message
#
message = MIMEMultipart()
body = "preview run complete. results attached"
message.attach(MIMEText(body))
#
# Get results file, zip it and attach to the email
#
resultsFileLocation = os.environ['TFB_REPOPARENT'] + "/" \
    + os.environ['TFB_REPONAME'] + "/results/latest/results.json"
temporaryFile = tempfile.TemporaryFile(prefix='results', suffix='.zip')
zipFile = zipfile.ZipFile(temporaryFile, 'w')
zipFile.write(resultsFileLocation)
zipFile.close()
temporaryFile.seek(0)
#
#Set up the email to be sent to the mailing list
#
message['From'] = os.environ['TFB_MAILING_FROM']
message['To'] = os.environ['TFB_MAILINGLIST']
message['Date'] = formatdate(localtime=True)
message['Subject'] = subprocess.check_output(["git", "describe", "--always"])
#
# Zip in memory and email results
#
resultsZipped = MIMEBase('application', 'zip')
resultsZipped.set_payload(temporaryFile.read())
encoders.encode_base64(resultsZipped)
resultsZipped.add_header('Content-Disposition', 'attachment', \
  filename='results.json.zip')
message.attach(resultsZipped)

#
# Attach the .commit file
#
commitFileLocation = os.environ['TFB_REPOPARENT'] + "/" \
    + os.environ['TFB_REPONAME'] + "/results/.commit"
attachment = MIMEBase('application', 'octet-stream')
attachment.set_payload(open(commitFileLocation, "rb").read())
encoders.encode_base64(attachment)
attachment.add_header('Content-Disposition', 'attachment; filename=".commit"')
message.attach(attachment)

#
# Find and attach the metadata file
#
metadataLocation = find("test_metadata.json", os.environ['TFB_REPOPARENT'] + "/" \
    + os.environ['TFB_REPONAME'] + "/results")
if metadataLocation is not None:
  attachment = MIMEBase('application', 'octet-stream')
  attachment.set_payload(open(metadataLocation, "rb").read())
  encoders.encode_base64(attachment)
  attachment.add_header('Content-Disposition', 'attachment; filename="test_metadata.json"')
  message.attach(attachment)


#
# Send the message and close the collection
#
smtp = smtplib.SMTP('smtp.techempower.com')
smtp.sendmail(os.environ['TFB_MAILING_FROM'], os.environ['TFB_MAILINGLIST'], \
    message.as_string())
smtp.close()
