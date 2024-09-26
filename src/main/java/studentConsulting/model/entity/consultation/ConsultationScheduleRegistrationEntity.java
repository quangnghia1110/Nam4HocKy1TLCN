package studentConsulting.model.entity.consultation;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDateTime;

@Data
@Builder
@Entity
@Table(name = "consultation_schedule_registration")
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleRegistrationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserInformationEntity user;

    @ManyToOne
    @JoinColumn(name = "consultation_schedule_id", referencedColumnName = "id")
    private ConsultationScheduleEntity consultationSchedule;

    @Column(name = "registered_at")
    private LocalDateTime registeredAt;

    @Column(name = "status")
    private Boolean status;
}
