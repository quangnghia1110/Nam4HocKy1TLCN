package studentConsulting.model.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;

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

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ConsultationScheduleRegistrationEntity that = (ConsultationScheduleRegistrationEntity) o;

        return Objects.equals(id, that.id);
    }
}
