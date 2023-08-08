
from sqlalchemy.dialects.postgresql.base import PGDialect; PGDialect._get_server_version_info = lambda *args: (9, 2)
from dataHub import dataHub
dh = dataHub()

# db_con = dh.db_connect('postgres')
db_con = dh.db_connect('cockroach')
fty_con = dh.fty_api_con()

# Function to return dict with success or failure message
def job_logger(func):
  try:
    func
  except BaseException as e:
    return 'Failure: ' + e
  else:
    return 'Success'

# Update statements
job_log = dict()
job_log['free_agents'] = job_logger(dh.fty_get_free_agents(fty_con.free_agents(size=1000), db_con))
