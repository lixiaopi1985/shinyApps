# -*- coding: utf-8 -*-
"""
Created on Mon May 21 16:57:29 2018

@author: swaggyp1985
"""

import requests
from requests.exceptions import RequestException
from contextlib import closing


def simple_get(url):
    
    try:
        with closing(requests.get(url, stream = True)) as resp:
            if is_good_response(resp):
                return resp.content
            else:
                return None
                
    except RequestException as e:
        log_error('Error during requests to {0} : {1}'.format(url, str(e)))
        return None
        
        
def is_good_response(resp):
    """
    returns true if the response seems to be HTML, false otherwise
    """
    
    content_type = resp.headers['Content-Type'].lower()
    return (resp.status_code == 200
            and content_type is not None
            and content_type.find('html') > -1)
            

def log_error(e):
    """
    It is always a good idea to log errors.
    This function just prints them, but you can make it do anything.
    """
    print(e)
    
    
if __name__ == '__main__':
    simple_get()
    is_good_response()
    log_error()
    