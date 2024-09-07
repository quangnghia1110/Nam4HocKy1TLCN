package studentConsulting.model.entity.feedback;

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
@Table(name = "ratings")
@NoArgsConstructor
@AllArgsConstructor

public class RatingEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity user; 

    @ManyToOne
    @JoinColumn(name = "consultant_id", nullable = true, referencedColumnName = "id")
    private UserInformationEntity consultant;

    @ManyToOne
    @JoinColumn(name = "department_id", nullable = false, referencedColumnName = "id")
    private DepartmentEntity department; 
    
    @Column(name = "general_satisfaction", nullable = false)
    private int generalSatisfaction; 

    @Column(name = "general_comment", columnDefinition = "TEXT")
    private String generalComment;  

    @Column(name = "expertise_knowledge", nullable = false)
    private int expertiseKnowledge;  

    @Column(name = "expertise_comment", columnDefinition = "TEXT")
    private String expertiseComment;  

    @Column(name = "attitude", nullable = false)
    private int attitude;  

    @Column(name = "attitude_comment", columnDefinition = "TEXT")
    private String attitudeComment; 

    @Column(name = "response_speed", nullable = false)
    private int responseSpeed;  

    @Column(name = "response_speed_comment", columnDefinition = "TEXT")
    private String responseSpeedComment;  

    @Column(name = "understanding", nullable = false)
    private int understanding;  

    @Column(name = "understanding_comment", columnDefinition = "TEXT")
    private String understandingComment; 

    @Column(name = "submitted_at", nullable = false)
    private LocalDateTime submittedAt;  
}


