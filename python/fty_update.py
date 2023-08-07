
from dataHub import dataHub
dh = dataHub()

# db_con = dh.db_connect('postgres')
# db_con = dh.db_connect('cockroach')
# fty_con = dh.fty_api_con()

# Add update statements here.
# Somehow return dict with success or failure message
# so it can be displayed in popup box on front end
job_log = dict()
# dh.fty_get_free_agents(fty_con.free_agents(size=1000), db_con)
job_log['test1'] = dh.test1()
job_log['test2'] = dh.test2()

