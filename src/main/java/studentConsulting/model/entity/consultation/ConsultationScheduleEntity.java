package studentConsulting.model.entity.consultation;
import java.time.LocalDate;
import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;

@Data
@Builder
@Entity
@Table(name = "consultation_schedule")
@NoArgsConstructor
@AllArgsConstructor
public class ConsultationScheduleEntity {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = true, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = true, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = true, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private LocalDateTime updatedAt;
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity user; 

    @ManyToOne
    @JoinColumn(name = "consultant_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity consultant; 

    @Column(name = "title", nullable = true, length = 255)
    private String title;

    @Column(name = "content", nullable = true, length = 255)
    private String content; 

    @Column(name = "consultation_date", nullable = true)
    private LocalDate consultationDate; 

    @Column(name = "consultation_time", nullable = true)
    private String consultationTime; 

    @Column(name = "location", nullable = true, length = 255)
    private String location;

    @Column(name = "link", nullable = true, length = 255)
    private String link; 

    @Column(name = "mode", nullable = true)
    private Boolean mode; 

    @Column(name = "status_confirmed", nullable = true)
    private Boolean statusConfirmed; 

    @Column(name = "status_public", nullable = true)
    private Boolean statusPublic; 
    
    @ManyToOne
    @JoinColumn(name = "department_id", nullable = true, referencedColumnName = "id")
    private DepartmentEntity department;
}
