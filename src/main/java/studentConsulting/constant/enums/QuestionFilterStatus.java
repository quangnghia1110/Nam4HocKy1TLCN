package studentConsulting.constant.enums;

public enum QuestionFilterStatus {
    ANSWERED("ANSWERED", "Câu hỏi đã trả lời"),
    NOT_ANSWERED("NOT_ANSWERED", "Câu hỏi chưa trả lời"),
    PRIVATE("PRIVATE", "Câu hỏi riêng tư"),
    PUBLIC("PUBLIC", "Câu hỏi công khai"),
    DELETED("DELETED", "Câu hỏi đã xóa"),
    APPROVED("APPROVED", "Câu trả lời yêu cầu phê duyệt");

    private final String key;
    private final String displayName;

    QuestionFilterStatus(String key, String displayName) {
        this.key = key;
        this.displayName = displayName;
    }

    public static QuestionFilterStatus fromKey(String key) {
        for (QuestionFilterStatus status : QuestionFilterStatus.values()) {
            if (status.getKey().equalsIgnoreCase(key)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown status key: " + key);
    }

    public String getKey() {
        return key;
    }

    public String getDisplayName() {
        return displayName;
    }
}
