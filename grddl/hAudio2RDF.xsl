<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns="http://xmlns.com/foaf/0.1/"
	xmlns:mo="http://purl.org/ontology/mo/"
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:dc="http://purl.org/dc/elements/1.1/"
	xmlns:xhtml="http://www.w3.org/1999/xhtml"
	xmlns:geo="http://www.w3.org/2003/01/geo/wgs84_pos#"
	xmlns:tl="http://purl.org/NET/c4dm/timeline.owl#"
	xmlns:foaf="http://xmlns.com/foaf/0.1/"
	xmlns:time="http://www.w3.org/2006/time#"
	exclude-result-prefixes="xhtml"
        version="1.0">

<xsl:strip-space elements="*"/>
<xsl:param name='base-uri' select="/*//xhtml:a[@rel='index']/@href"/>
<xsl:output method="xml" indent="yes" />

<xsl:param name="id" select="normalize-space(.//xhtml:*[@class=&quot;vcard&quot;]/@id)" />

<xsl:template match="/xhtml:html">
<rdf:RDF>

	<PersonalProfileDocument rdf:about="{$base-uri}">
		<maker rdf:resource="{$base-uri}#{$id}"/>
	</PersonalProfileDocument>

	<xsl:call-template name="person"/>

	<xsl:call-template name="haudio"/>

</rdf:RDF>
</xsl:template>

<xsl:template name="person">
	<Person rdf:ID="{$id}">
	<homepage rdf:resource="{$base-uri}"/>
	<name><xsl:value-of select="normalize-space(.//xhtml:*[@class=&quot;fn&quot;])"/></name>
	<mbox><xsl:value-of select="substring-after(normalize-space(.//xhtml:*[@class=&quot;email&quot;]/@href),'mailto:')"/></mbox>
	<xsl:call-template name="photo"/>
	<xsl:call-template name="geo"/>
	<xsl:call-template name="tracks"/>
	</Person>
</xsl:template>

<xsl:template name="photo">
		<xsl:if test=".//xhtml:*[@class=&quot;vcard&quot;]/descendant::xhtml:img[@class=&quot;photo&quot;] !=  0">
		<depiction rdf:resource="{//xhtml:*[@class=&quot;vcard&quot;]/descendant::xhtml:img[@class=&quot;photo&quot;]/@src}" />
		</xsl:if>
</xsl:template>

<xsl:template name="geo">
	<xsl:if test=".//xhtml:*[@class=&quot;geo&quot;] != 0">
	<based_near geo:lat="{normalize-space(.//xhtml:*[@class=&quot;latitude&quot;]/@title)}" geo:long="{normalize-space(.//xhtml:*[@class=&quot;longitude&quot;]/@title)}"/>
	</xsl:if>
</xsl:template>

<xsl:template name="tracks">
      <xsl:for-each select="./xhtml:body//xhtml:*[@class=&quot;haudio&quot;]">
      <interest>
		<mo:Record rdf:about="{descendant::xhtml:a[@rel=&quot;bookmark&quot;]/@href}">
		<dc:title><xsl:value-of select="descendant::xhtml:a[@rel=&quot;bookmark&quot;]"/></dc:title>
		<xsl:call-template name="track"/>
		</mo:Record>
      </interest>
      </xsl:for-each>
</xsl:template>

<xsl:template name="track">
	<mo:track rdf:resource="{descendant::xhtml:a[@rel=&quot;enclosure&quot;]/@href}"/>
</xsl:template>

<xsl:template name="haudio">
	<xsl:for-each select="./xhtml:body//xhtml:*[@class=&quot;haudio&quot;]">
	<mo:Track rdf:about="{.//xhtml:a[@rel=&quot;enclosure&quot;]/@href}">
		<dc:title><xsl:value-of select="normalize-space(.//xhtml:a[@rel=&quot;enclosure&quot;])"/></dc:title>
		<xsl:if test=".//xhtml:*[@class=&quot;description&quot;] != 0">
        	<dc:description><xsl:value-of select="normalize-space(.//xhtml:*[@class=&quot;description&quot;])"/></dc:description>
		</xsl:if>
		<mo:freedownload rdf:resource="{.//xhtml:a[@rel=&quot;enclosure&quot;]/@href}"/>
		<foaf:maker>
			<mo:MusicGroup rdf:about="{.//xhtml:*[contains(concat(' ',normalize-space(@class),' '),' url ')]/@href}">
				<img rdf:resource="{.//xhtml:img[@class=&quot;photo&quot;]/@src}"/>
				<name><xsl:value-of select="normalize-space(.//xhtml:*[@class=&quot;contributor&quot;]/.)" /></name>
				<homepage rdf:resource="{.//xhtml:*[contains(concat(' ',normalize-space(@class),' '),' url ')]/@href}"/>
			</mo:MusicGroup>
		</foaf:maker>
		<mo:time>
		   	<tl:Interval>
   			<tl:durationXSD>PT<xsl:call-template name="min"/>M<xsl:call-template name="second"/>S</tl:durationXSD>
			</tl:Interval>
		</mo:time>
	</mo:Track>
	</xsl:for-each>
</xsl:template>

<xsl:template name="second">
	<xsl:value-of select="substring-after(normalize-space(.//xhtml:*[@class=&quot;duration&quot;]),':')"/>
</xsl:template>

<xsl:template name="min">
	<xsl:value-of select="substring-before(normalize-space(.//xhtml:*[@class=&quot;duration&quot;]),':')"/>
</xsl:template>

</xsl:stylesheet>
