
import configparser
import snakecase
import espn_api.basketball as bb
from os import getcwd
from pathlib import Path
from requests import get
from datetime import datetime, date, timedelta
from pandas import DataFrame, concat, read_sql_query, to_datetime
from sqlalchemy import create_engine
from importlib.machinery import SourceFileLoader


class dataHub:
    
    def __init__(self):
        self
        
    def db_connect(self, platform):
        """ Establish connection to desired platform """
        
        parser = configparser.ConfigParser()
        parser.read(getcwd() + '/database.ini')
        db_creds = dict(parser.items(platform))
        sql_url = 'dialect://user:password@host:port/database'
        for el in db_creds: sql_url = sql_url.replace(el, db_creds[el])
        
        return create_engine(sql_url)
        

    def fty_api_con(self):
        """ Create connection object to fanstasy api """
        
        parser = configparser.ConfigParser()
        parser.read(getcwd() + '/database.ini')
        fty_creds = dict(parser.items('fantasy_api'))
        
        return bb.League(
            league_id=int(fty_creds['league_id']),
            year=int(fty_creds['year']),
            espn_s2=fty_creds['espn_s2'],
            swid=fty_creds['swid']
        )
        
    
    def fty_get_free_agents(self, players, db_con):
      
        df = []
        for player in players:
            df.append({
                'player_id': player.playerId,
                'player_name': player.name,
                'player_team': player.proTeam,
                'player_status': player.injuryStatus,
                'player_position': player.position
            })

        # Clean in prep for database ingestion
        df = DataFrame(df)
        
        # Write to database
        df.to_sql('free_agents', db_con, schema='fty', index=False, if_exists='replace')
        print('free_agents has been updated')
        # return df # Eventually delete


    def fty_get_team_rosters(self, players, postgres):
        pass
      
      
