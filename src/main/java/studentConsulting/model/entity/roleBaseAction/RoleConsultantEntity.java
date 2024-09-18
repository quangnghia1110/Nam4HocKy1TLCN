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
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

import java.sql.Timestamp;
import java.time.LocalDate;
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
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

	@ManyToOne
    @JoinColumn(name = "role_id",  referencedColumnName = "id")
    private RoleEntity role; 
	
    @Column(name = "name",  length = 50)
    private String name; 

    @OneToMany(mappedBy = "roleConsultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AnswerEntity> answers;
    
    @OneToMany(mappedBy = "roleConsultant", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AccountEntity> accounts;
}
