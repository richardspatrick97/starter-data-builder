package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.terms.TinkarTerm;

import java.lang.reflect.Field;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TinkarTermFilter {

    public static void main(String[] args){
        StringBuilder sb = new StringBuilder();
        sb.append("assemblage").append("|");
        sb.append("cvx").append("|");
        sb.append("clinvar").append("|");
        sb.append("rxnorm").append("|");
        sb.append("snomed").append("|");
        sb.append("loinc").append("|");
        sb.append("regenstrief").append("|");
        sb.append("string_").append("|");
        sb.append("statement_").append("|");
        sb.append("livd").append("|");
        sb.append("specimen").append("|");
        sb.append("skin").append("|");
        sb.append("skos").append("|");
        sb.append("nurse").append("|");
        sb.append("query_clause").append("|");
        sb.append("integer_").append("|");
        sb.append("oid").append("|");
        sb.append("mvx").append("|");
        sb.append("performance").append("|");
        sb.append("ndc").append("|");
        sb.append("medication").append("|");
        sb.append("nucc").append("|");
        sb.append("laterality").append("|");
        sb.append("intervention").append("|");
        sb.append("icd").append("|");
        sb.append("ihtsdo").append("|");
        sb.append("iso").append("|");
        sb.append("hl7").append("|");
        sb.append("groovy").append("|");
        sb.append("git").append("|");
        sb.append("gem").append("|");
        sb.append("fhir").append("|");
        sb.append("dynamic").append("|");
        sb.append("ibdf").append("|");
        sb.append("cpt").append("|"); //
        sb.append("action").append("|");
        sb.append("body").append("|");
        sb.append("automation").append("|");
        sb.append("business").append("|");
        sb.append("database").append("|");
        sb.append("datastore").append("|");
        sb.append("dose").append("|");
        sb.append("informatics").append("|");
        sb.append("image").append("|");
        sb.append("ingredient").append("|");
        sb.append("manifold").append("|");//
        sb.append("rf2").append("|");
        sb.append("milligram").append("|");
        sb.append("mapping").append("|");
        sb.append("vaccine").append("|");
        sb.append("vuid").append("|");
        sb.append("vhat").append("|");
        sb.append("va_station").append("|");
        sb.append("upper_bound").append("|");
        sb.append("circumstance").append("|");
        sb.append("unmappable").append("|");
        sb.append("units_different").append("|");
        sb.append("unicode").append("|");
        sb.append("units_different").append("|");
        sb.append("us_government").append("|");
        sb.append("us_extension").append("|");
        sb.append("timing").append("|");
        sb.append("template").append("|");//working
        sb.append("veterinary").append("|");
        sb.append("unmodeled").append("|");
        sb.append("test_").append("|");
        sb.append("systems").append("|");
        sb.append("synchronization").append("|");
        sb.append("symmetric").append("|");
        sb.append("subject").append("|");
        sb.append("source").append("|");
        sb.append("signify").append("|");
        sb.append("sequence").append("|");
        sb.append("sopt").append("|");
        sb.append("sctid").append("|");
        sb.append("sh_profile").append("|");
        sb.append("requested").append("|");
        sb.append("request").append("|");
        sb.append("represents").append("|");
        sb.append("replacement").append("|");
        sb.append("repetition").append("|");
        sb.append("reflexive").append("|");
        sb.append("query").append("|");
        sb.append("result").append("|");
        sb.append("purpose").append("|");
        sb.append("promotion").append("|");
        sb.append("procedure").append("|");
        sb.append("priority").append("|");//working
        sb.append("apache").append("|");
        sb.append("association").append("|");
        sb.append("boss").append("|");
        sb.append("broad").append("|");
        sb.append("_internal").append("|");
        sb.append("column").append("|");
        sb.append("communicate").append("|");
        sb.append("condor").append("|");
        sb.append("configuration").append("|");
        sb.append("content").append("|");
        sb.append("converter").append("|");
        sb.append("copyright").append("|");
        sb.append("count").append("|");
        sb.append("createive_commons").append("|");
        sb.append("current").append("|");
        sb.append("event").append("|");
        sb.append("existential").append("|");
        sb.append("external").append("|");
        sb.append("feature").append("|");
        sb.append("finding").append("|");
        sb.append("force").append("|");
        sb.append("generated").append("|");
        sb.append("health_risk").append("|");
        sb.append("immediate").append("|");
        sb.append("immutablecoordinate").append("|");
        sb.append("intrinsic").append("|");//working
        sb.append("withdrawn").append("|");
        sb.append("value").append("|");
        sb.append("values").append("|");
        sb.append("substance").append("|");
        sb.append("target").append("|");
        sb.append("precedence").append("|");
        sb.append("position").append("|");
        sb.append("polymorphic").append("|");
        sb.append("organism").append("|");
        sb.append("observation").append("|");
        sb.append("native").append("|");
        sb.append("ncbi").append("|");
        sb.append("mode").append("|");
        sb.append("milimeters").append("|");
        sb.append("metadata").append("|");
        sb.append("measurement").append("|");
        sb.append("map").append("|");
        sb.append("maps").append("|");
        sb.append("lineage").append("|");
        sb.append("item").append("|");
        sb.append("^type").append("|");
        sb.append("^name").append("|");
        sb.append("umls").append("|");
        sb.append("properties").append("|");//working
        sb.append("participant").append("|");
        sb.append("normal").append("|");
        sb.append("narrative").append("|");
        sb.append("marked").append("|");
        sb.append("long_").append("|");
        sb.append("code").append("|");
        sb.append("committed").append("|");//working 1:36 pm

        sb.append("pane").append("|");
        sb.append("panel").append("|");
        sb.append("nodes").append("|");
        sb.append("node").append("|");
        sb.append("taxonomy").append("|");
        sb.append("persona").append("|");
        sb.append("window").append("|");
        sb.append("classifier").append("|");
        sb.append("solor").append("|");//working 1:42

        sb.append("property").append("|");
        sb.append("prescribable").append("|");
        sb.append("period").append("|");
        sb.append("quality_assurance").append("|");
        sb.append("resolution").append("|");
        sb.append("measure").append("|");
        sb.append("lower").append("|");
        sb.append("filter").append("|");
        sb.append("field_1").append("|");
        sb.append("field_2").append("|");
        sb.append("field_substitution").append("|");
        sb.append("equivalence").append("|");
        sb.append("enable").append("|");
        sb.append("effective").append("|");
        sb.append("editor").append("|");
        sb.append("component_id_1").append("|");
        sb.append("component_id_2").append("|");
        sb.append("component_id_3").append("|");
        sb.append("component_id_4").append("|");
        sb.append("component_id_5").append("|");
        sb.append("component_id_6").append("|");
        sb.append("component_id_7").append("|");
        sb.append("routine").append("|");
        sb.append("allergen");

        System.out.println("start");
        int totalCount = 0;
        int neededCount = 0;
        for(Field field : TinkarTerm.class.getDeclaredFields()){

            Pattern pattern = Pattern.compile(sb.toString());
            Matcher matcher = pattern.matcher(field.getName().toLowerCase());

            if (!field.getAnnotatedType().getType().getTypeName().equals("dev.ikm.tinkar.terms.EntityProxy$Pattern")) {
                if (!matcher.find()){
//                    System.out.println(field.getName());

                    StringBuilder sb2 = new StringBuilder();

                    sb2.append("starterData.concept(TinkarTerm." + field.getName() + ")").append("\n");
                    sb2.append(".fullyQualifiedName(TinkarTerm." + field.getName() + ".description(), TinkarTerm.PREFERRED" + ")").append("\n");
                    sb2.append(".synonym(TinkarTerm." + field.getName() + ".description(), TinkarTerm.PREFERRED)").append("\n");
                    sb2.append(".definition(TinkarTerm."+ field.getName() + ".description(), TinkarTerm.PREFERRED)").append("\n");
                    sb2.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm." + field.getName() + ".asUuidArray()[0].toString())").append("\n");
//                    sb2.append(".statedDefinition(TinkarTerm.SOLOR_CONCEPT)").append("\n");
                    sb2.append(".inferredNavigation(null, null)").append("\n");
                    sb2.append(".statedNavigation(null, null)").append("\n");
                    sb2.append(".statedDefinition(TinkarTerm.ROOT_VERTEX)").append("\n");
                    sb2.append(".build();").append("\n");

                    System.out.println(sb2);



//                    System.out.println("starterData.concept(TinkarTerm." + field.getName() + ").fullyQualifiedName(TinkarTerm." + field.getName() + ".description(), TinkarTerm.PREFERRED).build();");
                    neededCount++;
                }
            }
            totalCount++;
        }

        StringBuilder identifierSourceSB = new StringBuilder();
        identifierSourceSB.append("starterData.concept(TinkarTerm.IDENTIFIER_SOURCE)").append("\n");
        identifierSourceSB.append(".fullyQualifiedName(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED" + ")").append("\n");
        identifierSourceSB.append(".synonym(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED)").append("\n");
        identifierSourceSB.append(".definition(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED)").append("\n");
        identifierSourceSB.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IDENTIFIER_SOURCE.asUuidArray()[0].toString())").append("\n");
//                    sb2.append(".statedDefinition(TinkarTerm.SOLOR_CONCEPT)").append("\n");
        identifierSourceSB.append(".inferredNavigation(null, null)").append("\n");
        identifierSourceSB.append(".statedNavigation(null, null)").append("\n");
        identifierSourceSB.append(".statedDefinition(TinkarTerm.ROOT_VERTEX)").append("\n");
        identifierSourceSB.append(".build();").append("\n");
        System.out.println(identifierSourceSB);

        StringBuilder versionPropertiesSB = new StringBuilder();
        versionPropertiesSB.append("starterData.concept(TinkarTerm.VERSION_PROPERTIES)").append("\n");
        versionPropertiesSB.append(".fullyQualifiedName(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED" + ")").append("\n");
        versionPropertiesSB.append(".synonym(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED)").append("\n");
        versionPropertiesSB.append(".definition(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED)").append("\n");
        versionPropertiesSB.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERSION_PROPERTIES.asUuidArray()[0].toString())").append("\n");
//                    sb2.append(".statedDefinition(TinkarTerm.SOLOR_CONCEPT)").append("\n");
        versionPropertiesSB.append(".inferredNavigation(null, null)").append("\n");
        versionPropertiesSB.append(".statedNavigation(null, null)").append("\n");
        versionPropertiesSB.append(".statedDefinition(TinkarTerm.ROOT_VERTEX)").append("\n");
        versionPropertiesSB.append(".build();").append("\n");
        System.out.println(versionPropertiesSB);

        StringBuilder statusValueSB = new StringBuilder();
        statusValueSB.append("starterData.concept(TinkarTerm.STATUS_VALUE)").append("\n");
        statusValueSB.append(".fullyQualifiedName(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED" + ")").append("\n");
        statusValueSB.append(".synonym(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED)").append("\n");
        statusValueSB.append(".definition(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED)").append("\n");
        statusValueSB.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATUS_VALUE.asUuidArray()[0].toString())").append("\n");
//                    sb2.append(".statedDefinition(TinkarTerm.SOLOR_CONCEPT)").append("\n");
        statusValueSB.append(".inferredNavigation(null, null)").append("\n");
        statusValueSB.append(".statedNavigation(null, null)").append("\n");
        statusValueSB.append(".statedDefinition(TinkarTerm.ROOT_VERTEX)").append("\n");
        statusValueSB.append(".build();").append("\n");
        System.out.println(statusValueSB);

        System.out.println("Needed Count: " + neededCount);
        System.out.println("Total Count: " + totalCount);

    }
}
