import pandas as pd

def retrieve_metadata(data):
    df = pd.DataFrame(columns=['id', 'display_name', 'level', 'description', 'works_count', 'cited_by_count', 'wikipedia_url'])
    
    for record in data['results']:
        if 'wikipedia' in record['ids']:
            wikipedia_id = record['ids']['wikipedia']
        else:
            wikipedia_id = None

        df_aux = pd.DataFrame({'id':record['id'],
                               'display_name':record['display_name'],
                               'level':record['level'],
                               'description':record['description'],
                               'works_count':record['works_count'],
                               'cited_by_count':record['cited_by_count'],
                               'wikipedia_url':wikipedia_id
                              }, index=[0])

        df = pd.concat([df, df_aux], ignore_index=True)
    
    return(df)