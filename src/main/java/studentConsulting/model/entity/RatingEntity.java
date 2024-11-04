package studentConsulting.model.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
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
@Table(name = "rating")
@NoArgsConstructor
@AllArgsConstructor
@JsonIdentityInfo(
        generator = ObjectIdGenerators.PropertyGenerator.class,
        property = "id")
public class RatingEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    @JsonIgnore
    private UserInformationEntity user;

    @ManyToOne
    @JoinColumn(name = "consultant_id", referencedColumnName = "id")
    @JsonIgnore
    private UserInformationEntity consultant;

    @ManyToOne
    @JoinColumn(name = "department_id", referencedColumnName = "id")
    @JsonIgnore
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

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RatingEntity that = (RatingEntity) o;

        return Objects.equals(id, that.id);
    }
}


