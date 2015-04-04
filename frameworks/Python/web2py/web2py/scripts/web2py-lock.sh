chown -R nobody:nobody *.py
chown -R nobody:nobody gluon
chown -R nobody:nobody scripts
chown -R nobody:nobody applications/*/modules/
chown -R nobody:nobody applications/*/models/
chown -R nobody:nobody applications/*/controllers/
chown -R nobody:nobody applications/*/views/
chown -R nobody:nobody applications/*/static/
chown -R nobody:nobody applications/*/cron/

echo "unlock with chown -R www-data:www-data ./"
