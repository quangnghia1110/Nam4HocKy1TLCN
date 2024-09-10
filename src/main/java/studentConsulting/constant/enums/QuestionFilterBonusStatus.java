package studentConsulting.constant.enums;

public enum QuestionFilterBonusStatus {
    ANSWERED("ANSWERED", "Câu hỏi đã trả lời"),
    NOT_ANSWERED("NOT_ANSWERED", "Câu hỏi chưa trả lời"),
    PRIVATE("PRIVATE", "Câu hỏi riêng tư"),
    PUBLIC("PUBLIC", "Câu hỏi công khai"),
    DELETED("DELETED", "Câu hỏi đã xóa"),
    NOT_APPROVED("NOT_APPROVED", "Câu hỏi chưa duyệt"),
    APPROVED("APPROVED", "Câu hỏi đã duyệt");

    private final String key;
    private final String displayName;

    QuestionFilterBonusStatus(String key, String displayName) {
        this.key = key;
        this.displayName = displayName;
    }

    public String getKey() {
        return key;
    }

    public String getDisplayName() {
        return displayName;
    }

    public static QuestionFilterBonusStatus fromKey(String key) {
        for (QuestionFilterBonusStatus status : QuestionFilterBonusStatus.values()) {
            if (status.getKey().equalsIgnoreCase(key)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown status key: " + key);
    }
}
