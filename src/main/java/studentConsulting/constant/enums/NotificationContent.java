package studentConsulting.constant.enums;

public enum NotificationContent {
    NEW_ANSWER("Câu hỏi của bạn vừa được trả lời bởi %s"),
    REVIEW_ANSWER("Câu hỏi của bạn vừa được trả lời bởi %s"),
    REVIEW_ANSWER_CONSULTANT("Câu trả lời của bạn vừa được đánh giá bởi %s"),
    NEW_CHAT_PRIVATE("Bạn có tin nhắn mới từ %s"),
    NEW_CHAT_GROUP("Bạn có tin nhắn mới từ nhớm %s"),
    NEW_CONSULATION_SCHEDULE("Bạn vừa nhận được lịch tư vấn từ %s"),
    CONFIRM_CONSULATION_SCHEDULE("Lịch tư vấn của bạn đã được xác nhận bởi tư vấn viên bởi %s"),
    NEW_CONSULATION_REGISTRATION("Bạn vừa nhận được lượt đăng ký tham gia buổi tư vấn từ %s"),
    NEW_POST("Cóa bài viết mới được đăng lên bời %s"),
    APPROVE_POST("Bài viết gần đây của bạn đã được duyệt bởi %s"),
    NEW_QUESTION("Bạn có câu hỏi mới từ %s"),
    DELETE_QUESTION("Câu hỏi của bạn vừa bị xóa bởi %s");
    private final String template;

    NotificationContent(String template) {
        this.template = template;
    }

    public String formatMessage(String senderName) {
        return String.format(template, senderName);
    }
}
