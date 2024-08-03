package studentConsulting.model.entity.authentication;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ParticipantEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation.ForwardedInfoEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.feedback.ReviewEntity;
import studentConsulting.model.entity.news.NewsEntity;
import studentConsulting.model.entity.news.NewsShareEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import java.sql.Timestamp;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "user_information")
@NoArgsConstructor
@AllArgsConstructor
public class UserInformationEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    
    @Column(name = "student_code", length = 50, unique = true)
    private String studentCode; // Mã số sinh viên

    @Column(name = "school_name", length = 255)
    private String schoolName; // Tên trường

    @Column(name = "firstname", length = 50)
    private String firstName; // Tên

    @Column(name = "lastname", length = 50)
    private String lastName; // Họ

    @Column(name = "phone", nullable = false, length = 10, unique = true)
    private String phone; // Số điện thoại

    @Column(name = "avatar_url", length = 900)
    private String avatarUrl; // Đường dẫn ảnh đại diện

    @Column(name = "gender", nullable = false, length = 3)
    private String gender; // Giới tính

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "address_id", nullable = true) // Cho phép giá trị NULL
    @JsonBackReference
    private AddressEntity address;

    
    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false, referencedColumnName = "id")
    @JsonBackReference
    private AccountEntity account; // Mã tài khoản tham chiếu

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private Set<RoleAuthEntity> roleAuths;
    
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ConversationEntity> userConversations; // Các cuộc trò chuyện của người dùng

    @OneToMany(mappedBy = "consultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ConversationEntity> consultantConversations; // Các cuộc trò chuyện của tư vấn viên

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ParticipantEntity> participants; // Danh sách người tham gia trong các cuộc trò chuyện

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ConsultationScheduleEntity> userConsultations; // Lịch tư vấn của người dùng

    @OneToMany(mappedBy = "consultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ConsultationScheduleEntity> consultantConsultations; // Lịch tư vấn của tư vấn viên

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<RatingEntity> ratings; // Các đánh giá của người dùng

    @OneToMany(mappedBy = "sender", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardedInfoEntity> sentForwardedInfos; // Các thông tin đã chuyển tiếp của người dùng

    @OneToMany(mappedBy = "receiver", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ForwardedInfoEntity> receivedForwardedInfos; // Các thông tin nhận được từ người dùng
    
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsEntity> news; // Các thông tin nhận được từ news
    
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NotificationEntity> notifications; // Các thông tin nhận được từ notification

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<QuestionEntity> questions; // Các thông tin nhận được từ question
    
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<CommonQuestionEntity> commonQuestions; // Các thông tin nhận được từ common question
    
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<ReviewEntity> reviews; // Các thông tin nhận được từ review
    
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<NewsShareEntity> newsShares; // Các thông tin nhận được từ news share
}

