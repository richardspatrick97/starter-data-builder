package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.terms.EntityProxy;

public class BMIConcept {
    private String snomedctId;
    private String decription;

    public String getSnomedctId() {
        return snomedctId;
    }

    public String getDecription() {
        return decription;
    }

    public BMIConcept(String snomedctId, String decription) {
        this.snomedctId = snomedctId;
        this.decription = decription;
    }

    public EntityProxy.Concept makeConcept(){
        return  EntityProxy.Concept.make(this.decription, UuidUtil.fromSNOMED(this.snomedctId));
    }



}
