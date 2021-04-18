"""ETL script for getting datasets from CDC Socrata
API.

WORK IN PROGRESS
"""
import json
import pandas as pd

from datetime import datetime
from sodapy import Socrata
from typing import Dict, NewType, Any


CredsT = NewType("CredsT", Dict[str, str])
SOC_DOMAIN = "data.cdc.gov"

def load_creds(fil: str) -> CredsT:
    """Loads JSON socrata API creds file.
    Currently the only required key is `app_token` which you will
    receive from your socrata API account.
    """
    dat = None
    with open(fil, 'rt') as fh:
        dat = json.load(fh)
    return dat


def get_dataset_meta(identifier: str, creds: CredsT) -> Dict[str, Any]:
    with Socrata(SOC_DOMAIN, creds["app_token"]) as client:
        res = client.datasets(limit=1, ids=[identifier])[0]
        #print(res['resource']['data_updated_at'])
        return res


def extract_data_to_df(identity: str, creds: CredsT) -> pd.DataFrame:
    """Extract data from CDC socrata API based on the
    identifier.
    """
    with Socrata(SOC_DOMAIN, creds["app_token"]) as client:
        records = client.get_all(identity, content_type="json")
        df = pd.DataFrame.from_dict(list(records))
        return df
        #df.to_csv(outpath, na_rep="NA")


if __name__ == '__main__':
    creds = load_creds("/Users/kylehernandez/Projects/Other/COV-IRT/cdc-data/app_creds.json")
    #meta = get_dataset_meta("kn79-hsxy", creds) 
    #data_updated_str = meta["resource"]["data_updated_at"]
    #data_updated_str = "2020-07-01T18:11:05.000Z"
    #data_updated_dt = datetime.strptime(data_updated_str, '%Y-%m-%dT%H:%M:%S.%fZ')
    #data_updated_fmt = data_updated_dt.strftime('%Y-%m-%d')
    #print(data_updated_fmt, data_updated_dt.strftime('%U'))
