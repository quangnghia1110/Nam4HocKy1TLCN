package studentConsulting.model.entity.authentication;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation.ForwardedInfoEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.news.Comment;
import studentConsulting.model.entity.news.PostEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

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
	private LocalDate createdAt;

	@Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
	private LocalDate updatedAt;

	@Column(name = "student_code", length = 50, unique = true)
	private String studentCode; 

	@Column(name = "school_name", length = 255)
	private String schoolName; 

	@Column(name = "firstname", length = 50)
	private String firstName; 

	@Column(name = "lastname", length = 50)
	private String lastName;

	@Column(name = "phone", nullable = false, length = 10, unique = true)
	private String phone; 

	@Column(name = "avatar_url", length = 900)
	private String avatarUrl; 

	@Column(name = "gender", nullable = false, length = 3)
	private String gender; 

	@ManyToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "address_id", nullable = true)
	@JsonIgnore
	private AddressEntity address;

	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<PostEntity> posts;
	@ManyToOne
	@JoinColumn(name = "account_id", nullable = false, referencedColumnName = "id")
	@JsonBackReference
	private AccountEntity account; 

	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	@JsonIgnore
	private Set<RoleAuthEntity> roleAuths;

	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<ConversationEntity> userConversations; 

	@ManyToOne
	@JsonBackReference
	@JoinColumn(name = "conversation_id")
	private ConversationEntity conversation;

	
	@OneToMany(mappedBy = "consultant", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<ConversationEntity> consultantConversations; 

	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<ConsultationScheduleEntity> userConsultations; 

	@OneToMany(mappedBy = "consultant", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<ConsultationScheduleEntity> consultantConsultations; 

	@OneToMany(mappedBy = "sender", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<ForwardedInfoEntity> sentForwardedInfos; 

	@OneToMany(mappedBy = "receiver", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<ForwardedInfoEntity> receivedForwardedInfos; 
	
	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<QuestionEntity> questions; 
	
	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<CommonQuestionEntity> commonQuestions; 

	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<AnswerEntity> answers;
	
	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<RatingEntity> ratingUsers;
	
	@OneToMany(mappedBy = "consultant", cascade = CascadeType.ALL, orphanRemoval = true)
	private Set<RatingEntity> ratingConsultants;
	
	@OneToMany(mappedBy = "userComment", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore 
    private List<Comment> comments;
	
	public String getName() {
        return this.lastName + " " + this.firstName;
    }
}
