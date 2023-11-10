package dev.ikm.tinkar.sandbox;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TinkarTermFilter {

    public static void main(String[] args){
        String conceptFilterStr = generateConceptFilter();
        Pattern pattern = Pattern.compile(conceptFilterStr);
        JsonNode jsonNode = readJSONResource("starter-data-concepts-resources.json");

        List<String> additionalTinkarTerms = List.of(
                "MODEL_CONCEPT",
                "IDENTIFIER_SOURCE",
                "VERSION_PROPERTIES",
                "STATUS_VALUE"
                );

        System.out.println("start");
        int totalCount = 0;
        int neededCount = 0;

        String uncatDestinationsList = "";

        for(Field field : TinkarTerm.class.getDeclaredFields()){
            Matcher matcher = pattern.matcher(field.getName().toLowerCase());

            if (!field.getAnnotatedType().getType().getTypeName().equals("dev.ikm.tinkar.terms.EntityProxy$Pattern")) {
                if (!matcher.find() || additionalTinkarTerms.contains(field.getName())){
//                    System.out.println(field.getName());

                    // Print concept definition code with **default values**
//                    printWithDefaultData(field);

                    // Print concept definition code with **supplied data**
                    JsonNode termNode = jsonNode.get(field.getName());

                    if (termNode.get("Origins").isEmpty()) {
                        uncatDestinationsList += "TinkarTerm." + field.getName() + ", ";
                    }

                    printWithSuppliedData(field, termNode);
                    neededCount++;
                }
            }
            totalCount++;
        }

        printUncategorizedGrouperDefinition(uncatDestinationsList.substring(0, uncatDestinationsList.length()-2));
        neededCount++;

        System.out.println("Needed Count: " + neededCount);
        System.out.println("Total Count: " + totalCount);
    }

    public static void printWithDefaultData(Field field) {
        StringBuilder starterDataDefaultPOJO = new StringBuilder();

        starterDataDefaultPOJO.append("starterData.concept(TinkarTerm." + field.getName() + ")").append("\n");
        starterDataDefaultPOJO.append(".fullyQualifiedName(TinkarTerm." + field.getName() + ".description(), TinkarTerm.PREFERRED" + ")").append("\n");
        starterDataDefaultPOJO.append(".synonym(TinkarTerm." + field.getName() + ".description(), TinkarTerm.PREFERRED)").append("\n");
        starterDataDefaultPOJO.append(".definition(TinkarTerm."+ field.getName() + ".description(), TinkarTerm.PREFERRED)").append("\n");
        starterDataDefaultPOJO.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm." + field.getName() + ".asUuidArray()[0].toString())").append("\n");
//        starterDataDefaultPOJO.append(".statedDefinition(TinkarTerm.SOLOR_CONCEPT)").append("\n");
        starterDataDefaultPOJO.append(".inferredNavigation(null, null)").append("\n");
        starterDataDefaultPOJO.append(".statedNavigation(null, null)").append("\n");
        starterDataDefaultPOJO.append(".statedDefinition(TinkarTerm.ROOT_VERTEX)").append("\n");
        starterDataDefaultPOJO.append(".build();").append("\n");

        System.out.println(starterDataDefaultPOJO);
    }

    public static void printWithSuppliedData(Field field, JsonNode termNode) {
        String destinations = getListOfOrDefault(termNode.get("Destinations"), "null");
        String origins = getListOfOrDefault(termNode.get("Origins"), "List.of(uncategorizedGrouper)");
        String statedDef = origins;

        if (field.getName() == "ROOT_VERTEX") {
            destinations = destinations.substring(0, destinations.length()-1) + ", uncategorizedGrouper" + ")";
            origins = "null";
            statedDef = "List.of(TinkarTerm.ROOT_VERTEX)";
        }

        StringBuilder starterDataPOJO = new StringBuilder();

        starterDataPOJO.append("starterData.concept(TinkarTerm." + field.getName() + ")").append("\n");
        starterDataPOJO.append(".fullyQualifiedName(\"" + termNode.get("FullyQualifiedName").asText() + "\", TinkarTerm.PREFERRED" + ")").append("\n");
        starterDataPOJO.append(".synonym(\"" + termNode.get("Synonym").asText() + "\", TinkarTerm.PREFERRED)").append("\n");
        starterDataPOJO.append(".definition(\"" + termNode.get("Definition").asText() + "\", TinkarTerm.PREFERRED)").append("\n");
        starterDataPOJO.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm." + field.getName() + ".asUuidArray()[0].toString())").append("\n");
        starterDataPOJO.append(".inferredNavigation(" + destinations + ", " + origins + ")").append("\n");
        starterDataPOJO.append(".statedNavigation(" + destinations + ", " + origins + ")").append("\n");
        starterDataPOJO.append(".statedDefinition(" + statedDef + ")").append("\n");
        starterDataPOJO.append(".build();").append("\n");

        System.out.println(starterDataPOJO);
    }

    private static String getListOfOrDefault(JsonNode node, String defaultStr) {
        String result = "";
        if (node.isEmpty()) {
            result = defaultStr;
        } else {
            result = "List.of(";
            for(JsonNode item : node) {
                result += "TinkarTerm." + item.asText() + ", ";
//                System.out.println(item.asText());
            }
            result = result.substring(0, result.length() - 2) + ")";
        }
        return result;
    }

    private static String generateConceptFilter() {
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

        return sb.toString();
    }

    private static JsonNode readJSONResource(String resourceFileName) {
        JsonNode jsonNode = null;
        try {
            // Read JSON resource
            ObjectMapper objectMapper = new ObjectMapper();
            InputStream inputStream = TinkarTermFilter.class.getClassLoader().getResourceAsStream(resourceFileName);
            jsonNode = objectMapper.readTree(inputStream);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
        return jsonNode;
    }

    private static void printUncategorizedGrouperDefinition(String destinationConceptList) {
        StringBuilder uncategorizedGrouperSB = new StringBuilder();

        uncategorizedGrouperSB.append("starterData.concept(uncategorizedGrouper)").append("\n");
        uncategorizedGrouperSB.append(".fullyQualifiedName(\"UNCATEGORIZED_GROUPER\", TinkarTerm.PREFERRED" + ")").append("\n");
        uncategorizedGrouperSB.append(".synonym(\"UNCATEGORIZED_GROUPER\", TinkarTerm.PREFERRED)").append("\n");
        uncategorizedGrouperSB.append(".definition(\"UNCATEGORIZED_GROUPER\", TinkarTerm.PREFERRED)").append("\n");
        uncategorizedGrouperSB.append(".identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODEL_CONCEPT.asUuidArray()[0].toString())").append("\n");
        uncategorizedGrouperSB.append(".inferredNavigation(List.of(" + destinationConceptList + "), List.of(TinkarTerm.ROOT_VERTEX))").append("\n");
        uncategorizedGrouperSB.append(".statedNavigation(List.of(" + destinationConceptList + "), List.of(TinkarTerm.ROOT_VERTEX))").append("\n");
        uncategorizedGrouperSB.append(".statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))").append("\n");
        uncategorizedGrouperSB.append(".build();").append("\n");

        System.out.println(uncategorizedGrouperSB);

//        starterData.concept(uncategorizedGrouper)
//                .fullyQualifiedName("UNCATEGORIZED_GROUPER", TinkarTerm.PREFERRED)
//                .synonym("UNCATEGORIZED_GROUPER", TinkarTerm.PREFERRED)
//                .definition("UNCATEGORIZED_GROUPER", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODEL_CONCEPT.asUuidArray()[0].toString())
//                .inferredNavigation(List.of(TinkarTerm.CASE_INSENSITIVE_EVALUATION, TinkarTerm.CASE_SENSITIVE_EVALUATION, TinkarTerm.GB_ENGLISH_DIALECT, TinkarTerm.HEALTH_CONCEPT, TinkarTerm.IS_A, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.OBJECT, TinkarTerm.PART_OF, TinkarTerm.PATH_ORIGINS_PATTERN, TinkarTerm.PATHS_PATTERN, TinkarTerm.ROLE_GROUP, TinkarTerm.SIGNED_INTEGER, TinkarTerm.STRING, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.US_ENGLISH_DIALECT, TinkarTerm.UUID_DATA_TYPE, TinkarTerm.VIEW_COORDINATE_KEY, TinkarTerm.IDENTIFIER_SOURCE, TinkarTerm.VERSION_PROPERTIES, TinkarTerm.STATUS_VALUE), List.of(TinkarTerm.ROOT_VERTEX))
//                .statedNavigation(List.of(TinkarTerm.CASE_INSENSITIVE_EVALUATION, TinkarTerm.CASE_SENSITIVE_EVALUATION, TinkarTerm.GB_ENGLISH_DIALECT, TinkarTerm.HEALTH_CONCEPT, TinkarTerm.IS_A, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.OBJECT, TinkarTerm.PART_OF, TinkarTerm.PATH_ORIGINS_PATTERN, TinkarTerm.PATHS_PATTERN, TinkarTerm.ROLE_GROUP, TinkarTerm.SIGNED_INTEGER, TinkarTerm.STRING, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.US_ENGLISH_DIALECT, TinkarTerm.UUID_DATA_TYPE, TinkarTerm.VIEW_COORDINATE_KEY, TinkarTerm.IDENTIFIER_SOURCE, TinkarTerm.VERSION_PROPERTIES, TinkarTerm.STATUS_VALUE), List.of(TinkarTerm.ROOT_VERTEX))
//                .statedDefinition(TinkarTerm.ROOT_VERTEX)
//                .build();
    }
}
