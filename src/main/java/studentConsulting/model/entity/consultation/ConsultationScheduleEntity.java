package studentConsulting.model.entity.consultation;
import java.time.LocalDate;
import java.time.LocalDate;

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
    @Column( name = "id")
    private Integer id;

    @Column(name = "created_at",  updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "user_id",  referencedColumnName = "id")
    private UserInformationEntity user; 

    @ManyToOne
    @JoinColumn(name = "consultant_id",  referencedColumnName = "id")
    private UserInformationEntity consultant; 

    @Column(name = "title",  length = 255)
    private String title;

    @Column(name = "content",  length = 255)
    private String content; 

    @Column(name = "consultation_date")
    private LocalDate consultationDate; 

    @Column(name = "consultation_time")
    private String consultationTime; 

    @Column(name = "location",  length = 255)
    private String location;

    @Column(name = "link",  length = 255)
    private String link; 

    @Column(name = "mode")
    private Boolean mode; 

    @Column(name = "status_confirmed")
    private Boolean statusConfirmed; 

    @Column(name = "status_public")
    private Boolean statusPublic; 
    
    @ManyToOne
    @JoinColumn(name = "department_id",  referencedColumnName = "id")
    private DepartmentEntity department;
}
