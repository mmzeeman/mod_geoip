{% extends "admin_base.tpl" %}

{% block title %} {_ Admin GeoIP _} {% endblock %}

{% block content %}
{% with m.acl.is_admin as is_editable %}
<div class="admin-header">
    <h2>{_ GeoIP _}</h2>
</div>

<div class="row">
    <div class="col-lg-8 col-md-6">
        <p>This product includes GeoLite2 data created by MaxMind, available from
        <a href="http://www.maxmind.com">http://www.maxmind.com</a>.
        </p>
    </div>
</div>

{# TODO: Add a way to update the maxmind database #}

<div class="row">
    <div class="col-lg-8 col-md-6">
        <p>Download and extract databases from 
        <a href="http://dev.maxmind.com/geoip/geoip2/geolite2/">http://dev.maxmind.com/geoip/geoip2/geolite2/</a>
        to zotonic's priv directory.</p>
    </div>
</div>


<div class="row">
    <div class="col-lg-8 col-md-6">
        <h2>Some Usage Examples</h2>
        <p>Your IP:
        {{ m.req.peer | pprint }}
        </p>
        <pre>
{{ m.geoip[m.req.peer]|default:"No information found" | pprint }}
        </pre>

<div class="row">
    <div class="col-lg-8 col-md-6">
{% with "95.211.188.8" as example_ip %}
        <p>Information on: {{ example_ip }} </p>
{% with m.geoip.country[example_ip] as geoinfo %}
        <pre>
{{ geoinfo | pprint }}
        </pre>
<ul>
    <li>Continent: {{ geoinfo.continent.name }}</li>
    <li>Country: {{ geoinfo.country.name }}</li>
</ul>
</div>
</div>

{% endwith %}
{% endwith %}

    </div>
</div>



{% endwith %}
{% endblock %}
