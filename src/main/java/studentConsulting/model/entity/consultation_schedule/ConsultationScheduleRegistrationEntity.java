package studentConsulting.model.entity.consultation_schedule;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDate;

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
    private LocalDate registeredAt;

    @Column(name = "status")
    private Boolean status;
}
