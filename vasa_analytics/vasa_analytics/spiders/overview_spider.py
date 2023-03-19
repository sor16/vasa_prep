from pathlib import Path

import scrapy

BASE_URL = "https://results.vasaloppet.se/2023/"

class OverviewSpider(scrapy.Spider):
    name = "overview"

    def start_requests(self):
        urls = [
            f'https://results.vasaloppet.se/2023/?page={i}&event=VL_HCH8NDMR2300&pid=search' for i in range(1,609)
        ]
        self.logger.info(f'URLS: {urls}')
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response):
        result_list = response.css('ul.list-group.list-group-multicolumn > li h4.type-fullname > a::attr(href)').getall()

        for result in result_list:
            yield scrapy.Request(BASE_URL + result, callback = self.parse_dir_contents)

        
    def parse_dir_contents(self, response):
        time_splits = response.css('tr.split')
        finish_time = response.css('tr.f-time_finish_brutto')

        time_splits.append(finish_time)

        output_json = dict()

        name = response.css('.f-__fullname.last::text').get()
        start_group = response.css('.f-start_group.last::text').get()
        self.logger.info(f'Name: {name}')

        output_json['name'] = name
        output_json['start_group'] = start_group

        important_cols = ['desc', 'time', 'diff', 'min_km', 'kmh', 'place']

        output_json['splits'] = []
        for split in time_splits:
            split_info = dict()
            for col in important_cols:
                split_info[col] = split.css(f".{col}::text").get()

            output_json['splits'].append(split_info)
        
        yield output_json

