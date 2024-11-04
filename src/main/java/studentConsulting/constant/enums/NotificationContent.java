package studentConsulting.constant.enums;

public enum NotificationContent {

    // Thông báo cho câu hỏi
    NEW_ANSWER("Câu hỏi của bạn đã được tư vấn viên %s trả lời."),
    REVIEW_ANSWER("Câu hỏi của bạn đã nhận được câu trả lời từ %s."),
    REVIEW_ANSWER_CONSULTANT("Câu trả lời của bạn đã được đánh giá bởi %s."),

    // Thông báo về tin nhắn
    NEW_CHAT_PRIVATE("Bạn có một tin nhắn mới từ %s trong cuộc trò chuyện riêng."),
    NEW_CHAT_GROUP("Bạn có một tin nhắn mới từ %s trong nhóm %s."),

    // Thông báo liên quan đến lịch tư vấn
    NEW_CONSULTATION_SCHEDULE("Bạn vừa nhận được lịch tư vấn mới từ %s. Vui lòng kiểm tra thông tin chi tiết."),
    NEW_CONSULTATION_SCHEDULE_ADMIN("Trưởng ban %s đã tạo một lịch tư vấn mới. Vui lòng xem chi tiết."),
    CONSULTATION_SCHEDULE_CONFIRMED("Lịch tư vấn của bạn đã được xác nhận bởi tư vấn viên %s."),
    NEW_CONSULTATION_PARTICIPANT("Buổi tư vấn của bạn có một người dùng mới tham gia là %s."),

    // Thông báo cho bài viết
    NEW_POST_CREATED("Tư vấn viên hoặc trưởng ban %s đã đăng một bài viết mới."),
    APPROVE_POST("Bài viết gần đây của bạn đã được phê duyệt bởi %s."),

    // Thông báo cho câu hỏi
    NEW_QUESTION("Bạn có một câu hỏi mới từ người dùng %s."),
    DELETE_QUESTION("Câu hỏi của bạn đã bị xóa bởi %s."),
    NEW_COMMON_QUESTION("Trưởng ban %s đã tạo một câu hỏi chung và đã chuyển tới admin để duyệt."),
    FORWARD_QUESTION_RECEIVED("Bạn đã nhận được câu hỏi chuyển tiếp từ tư vấn viên %s."),
    FORWARD_QUESTION_SENT("Câu hỏi của bạn đã được chuyển tiếp bởi %s tới bộ phận khác."),

    // Thông báo cho bình luận
    NEW_COMMENT("Bài viết của bạn đã nhận được bình luận mới từ %s."),
    NEW_REPLY_POST("Bài viết của bạn đã nhận được một phản hồi mới từ %s."),
    NEW_REPLY_COMMENT("Bình luận của bạn đã được phản hồi bởi %s."),

    // Thông báo cho lượt thích
    LIKE_POST("Bài viết của bạn đã được thích bởi %s."),
    LIKE_COMMENT("Bình luận của bạn đã được thích bởi %s."),

    // Thông báo cho cuộc trò chuyện
    NEW_CONVERSATION_CREATED("Một cuộc trò chuyện mới đã được tạo bởi %s."),

    // Thông báo cho đánh giá
    NEW_RATING_RECEIVED("Bạn đã nhận được một đánh giá từ người dùng %s.");

    private final String message;

    NotificationContent(String messgae) {
        this.message = messgae;
    }

    public String formatMessage(String senderName) {
        return String.format(message, senderName);
    }
}
