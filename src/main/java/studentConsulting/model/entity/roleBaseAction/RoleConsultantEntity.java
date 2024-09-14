package studentConsulting.model.entity.roleBaseAction;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.RoleAuthEntity;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation.ForwardedInfoEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import java.sql.Timestamp;
import java.util.Set;
@Data
@Builder
@Entity
@Table(name = "role_consultant")
@NoArgsConstructor
@AllArgsConstructor
public class RoleConsultantEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
	@ManyToOne
    @JoinColumn(name = "role_id", nullable = false, referencedColumnName = "id")
    private RoleEntity role; 
	
    @Column(name = "name", nullable = false, length = 50)
    private String name; 

    @OneToMany(mappedBy = "roleConsultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AnswerEntity> answers;
    
    @OneToMany(mappedBy = "roleConsultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AccountEntity> accounts;
}
