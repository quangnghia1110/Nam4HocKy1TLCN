package studentConsulting.model.entity.rating;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDate;

@Data
@Builder
@Entity
@Table(name = "rating")
@NoArgsConstructor
@AllArgsConstructor

public class RatingEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserInformationEntity user;

    @ManyToOne
    @JoinColumn(name = "consultant_id", referencedColumnName = "id")
    private UserInformationEntity consultant;

    @ManyToOne
    @JoinColumn(name = "department_id", referencedColumnName = "id")
    private DepartmentEntity department;

    @Column(name = "general_satisfaction")
    private int generalSatisfaction;

    @Column(name = "general_comment", columnDefinition = "TEXT")
    private String generalComment;

    @Column(name = "expertise_knowledge")
    private int expertiseKnowledge;

    @Column(name = "expertise_comment", columnDefinition = "TEXT")
    private String expertiseComment;

    @Column(name = "attitude")
    private int attitude;

    @Column(name = "attitude_comment", columnDefinition = "TEXT")
    private String attitudeComment;

    @Column(name = "response_speed")
    private int responseSpeed;

    @Column(name = "response_speed_comment", columnDefinition = "TEXT")
    private String responseSpeedComment;

    @Column(name = "understanding")
    private int understanding;

    @Column(name = "understanding_comment", columnDefinition = "TEXT")
    private String understandingComment;

    @Column(name = "submitted_at")
    private LocalDate submittedAt;
}


