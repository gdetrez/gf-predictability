<?xml version="1.0" encoding="UTF-8"?><!-- -*-  mode: xml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  
		version="1.0">

  <xsl:output method="text" encoding="utf-8"/>
  <xsl:strip-space elements="*" />
  
  <xsl:template match="/">
      <xsl:for-each select="Lexicon/LexicalEntry">
	<xsl:apply-templates select="." />
      </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="LexicalEntry">
    <xsl:if test="pos = 'vb'">
	<xsl:value-of select="saldo" />
	<xsl:text>,</xsl:text>

	<!-- CommonScand.VF (CommonScand.VPres CommonScand.Act) => äter -->
	<xsl:value-of select="table/form/wf[../param = 'pres ind aktiv']" />
	<xsl:text>,</xsl:text>

	<!-- CommonScand.VF (CommonScand.VPres CommonScand.Pass) => ätes -->
	<!-- More than one sometimes... what to do ? -->
	<xsl:value-of select="table/form/wf[../param = 'pres ind s-form']" />
	<xsl:text>,</xsl:text>

	
	<!-- CommonScand.VF (CommonScand.VPret CommonScand.Act) => åt -->
	<xsl:value-of select="table/form/wf[../param = 'pret ind aktiv']" />
	<xsl:text>,</xsl:text>

	<!-- CommonScand.VF (CommonScand.VPret CommonScand.Pass) => åts -->
	<xsl:value-of select="table/form/wf[../param = 'pret ind s-form']" />
	<xsl:text>,</xsl:text>

	<!-- CommonScand.VF (CommonScand.VImper CommonScand.Act) => ät -->
	<xsl:value-of select="table/form/wf[../param = 'imper']" />
	<xsl:text>,</xsl:text>

	<!-- CommonScand.VF (CommonScand.VImper CommonScand.Pass) -->
	<!-- not in saldom
	<xsl:value-of select="table/form/wf[../param = '']" />
	<xsl:text>,</xsl:text>
	 -->


	<!-- (VI (VInfin Act)) : gå -->
	<xsl:value-of select="table/form/wf[../param = 'inf aktiv']" />
	<xsl:text>,</xsl:text>

	<!-- (VI (VInfin Pass)) : gås -->
	<xsl:value-of select="table/form/wf[../param = 'inf s-form']" />
	<xsl:text>,</xsl:text>

	<!-- (VI (VSupin Act)) : gått -->
	<xsl:value-of select="table/form/wf[../param = 'sup aktiv']" />
	<xsl:text>,</xsl:text>

	<!-- (VI (VSupin Pass)) : gåtts -->
	<xsl:value-of select="table/form/wf[../param = 'sup s-form']" />
	<xsl:text>,</xsl:text>


	<!-- (VI (VPtPret (Strong (GSg Utr)) Nom)) : gången -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part indef sg u nom']" />
	<xsl:text>,</xsl:text>
	<!-- (VI (VPtPret (Strong (GSg Utr)) Gen)) : gångens -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part indef sg u gen']" />
	<xsl:text>,</xsl:text>

	<!-- not in Saldom: -->
	<!-- (VI (VPtPret (Strong (GSg Neutr)) Nom)) : gånget -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part indef sg n nom']" />
	<xsl:text>,</xsl:text>
	<!-- (VI (VPtPret (Strong (GSg Neutr)) Gen)) : gångets -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part indef sg n gen']" />
	<xsl:text>,</xsl:text>
	<!-- (VI (VPtPret (Strong GPl) Nom)) : gångna -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part indef pl nom']" />
	<xsl:text>,</xsl:text>
	<!-- (VI (VPtPret (Strong GPl) Gen)) : gångnas -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part indef pl gen']" />
	<xsl:text>,</xsl:text>
	<!-- (VI (VPtPret (Weak Sg) Nom)) : gångna -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part def sg no_masc nom']" />
	<xsl:text>,</xsl:text>
	<!-- (VI (VPtPret (Weak Sg) Gen)) : gångnas -->
	<xsl:value-of select="table/form/wf[../param = 'pret_part def sg no_masc gen']" />

	<!-- not in Saldom: -->
	<!-- (VI (VPtPret (Weak Pl) Nom)) : gångna -->
	<!-- (VI (VPtPret (Weak Pl) Gen)) : gångnas -->
	<xsl:text>&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
