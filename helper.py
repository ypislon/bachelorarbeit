from datetime import date, datetime, timedelta

def parse_articles_url(website):
    url_fragment = website.article_page
    urls = list()
    urls.append(url_fragment)
    if "$c$" in url_fragment and len(website.categories):
        urls_temp = list()
        for i, url in enumerate(urls):
            for category in website.categories:
                url_temp = url.replace("$c$", category.name)
                urls_temp.append(url_temp)
        # replace list of urls with new built list
        urls = urls_temp
    urls

    if "$x$" in url_fragment:
        urls_temp = list()
        for i, url in enumerate(urls):
            # TODO
            # for i in website.article_identifier_max:
            for j in range(1,5):
                url_temp = url.replace("$x$", str(j))
                urls_temp.append(url_temp)
        # replace list of urls with new built list
        urls = urls_temp
    urls

    # create bounds for the date
    # duration should be from 1.3.16 to 1.3.18
    start_date = date(2016, 3, 1)
    end_date = date(2016, 4, 1)

    if "$y$" in url_fragment:
        urls_temp = list()
        for year in (2016, 2017, 2018):
            for url in urls:
                url_temp = url.replace("$y$", str(year))
                urls_temp.append(url_temp) #etc...
        urls = urls_temp
    if "$m$" in url_fragment:
        urls_temp = list()
        for month in (1,2,3,4,5,6,7,8,9,10,11,12):
            for url in urls:
                if(month < 10):
                    month_string = "0" + str(month)
                else:
                    month_string = str(month)
                url_temp = url.replace("$m$", month_string)
                urls_temp.append(url_temp) #etc...
        urls = urls_temp
    if "$d$" in url_fragment:
        urls_temp = list()
        for temp_date in daterange(start_date, end_date):
            for url in urls:
                if(temp_date < 10):
                    date_string = "0" + str(temp_date)
                else:
                    date_string = str(temp_date)
                url_temp = url.replace("$d$", date_string)
                urls_temp.append(url_temp) #etc...
        urls = urls_temp
    return website, urls

def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)
