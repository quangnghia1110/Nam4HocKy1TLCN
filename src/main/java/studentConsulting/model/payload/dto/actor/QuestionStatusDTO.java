package studentConsulting.model.payload.dto.actor;

public class QuestionStatusDTO {
    private String key;
    private String displayName;

    public QuestionStatusDTO(String key, String displayName) {
        this.key = key;
        this.displayName = displayName;
    }

    // Getters và Setters
    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }
}

