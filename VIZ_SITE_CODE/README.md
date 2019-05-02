# Static Site image viewer.

In `VIZ SITE CODE` the folder `public html` contains a fully developed static site build through JavaScript, HTML and css designed to view the days forecast. This can be hosted on a web server or simply viewed via a light weight python web server.

* css - custom style files
* fonts - custom fonts
* images  - banners, icons, etc
* _includes - common html code to multiple pages e.g. naviation bar etc.
* index.html  - Landing page.
* js - Javascript library for animating, loading in the days images etc
* UNRESP_VIZ - folder containing image archive,

This code will work out of the box with `Run.sh` or can be adapted for other purposes.

## Viewing Output

The output can be viewed by running:

```bash
cd $HOME/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html
python -m http.server
```
And opening http://0.0.0.0:8000/ in any browser

All the code can be transported to desired location e.g. Apache server and the
forecasting scripts ran with a `-n` option to move to that location.
