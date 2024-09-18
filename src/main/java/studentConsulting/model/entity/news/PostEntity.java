package studentConsulting.model.entity.news;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import javax.persistence.*;
import java.time.LocalDate;

@Entity
@Table(name = "posts")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PostEntity {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    private String content;
    private boolean isAnonymous;
    private String fileName; 
    private boolean isApproved;
    private int views;
    private LocalDate createdAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id")
    private UserInformationEntity user; 

    public PostEntity(Integer id) {
        this.id = id;
    }
}
